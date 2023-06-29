use anyhow::Context;
use lelele_example_json::{
    lexer::{lexer, Token},
    parser::{ParserDef, SymbolID},
    syntax::{Array, EscapedStr, Object, Value},
};
use lelele_runtime::parser::ParseItem::*;
use lelele_runtime::parser::{ParseEvent, Parser};
use std::{env, fs};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(tracing::Level::TRACE)
        .init();

    let input_path = env::args().nth(1).context("missing input name")?;
    let input = fs::read_to_string(&input_path).context("read JSON file")?;
    let mut tokens = lexer(&input);

    let mut parser = Parser::new(ParserDef::default());

    enum StackItem<'source> {
        Value(Value<'source>),
        Object(Object<'source>),
        Array(Array<'source>),
        Member((&'source EscapedStr, Box<Value<'source>>)),
        Members(Vec<(&'source EscapedStr, Box<Value<'source>>)>),
        Elements(Vec<Box<Value<'source>>>),
    }
    let mut stack = vec![];
    macro_rules! pop_stack_item {
        ($variant:ident) => {{
            match stack.pop() {
                Some(StackItem::$variant(v)) => v,
                _ => anyhow::bail!("unexpected stack item"),
            }
        }};
    }

    let parsed = loop {
        let span = tracing::trace_span!("resume");
        let _entered = span.enter();

        let event = parser.resume().map_err(|e| {
            eprintln!("parse error: {:?}", e);
            e
        })?;
        match event {
            ParseEvent::InputNeeded => match tokens.next() {
                Some(tok) => {
                    let tok = tok?;
                    tracing::trace!("offer token {:?}", tok);
                    parser.offer_token(tok)?;
                }
                None => {
                    tracing::trace!("offer end of input");
                    parser.offer_eoi()?;
                }
            },

            ParseEvent::Shifting(lookahead) => {
                tracing::trace!("shift: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(symbol, args) => {
                tracing::trace!("reduce: {:?} -> {:?}", symbol, args);
                match (symbol, args) {
                    (SymbolID::VALUE, [T(Token::Null)]) => {
                        stack.push(StackItem::Value(Value::Null));
                    }
                    (SymbolID::VALUE, [T(Token::True)]) => {
                        stack.push(StackItem::Value(Value::Bool(true)));
                    }
                    (SymbolID::VALUE, [T(Token::False)]) => {
                        stack.push(StackItem::Value(Value::Bool(false)));
                    }
                    (SymbolID::VALUE, [T(Token::Number(number))]) => {
                        stack.push(StackItem::Value(Value::Number(number)));
                    }
                    (SymbolID::VALUE, [T(Token::String(raw))]) => {
                        stack.push(StackItem::Value(Value::String(EscapedStr::from_raw(
                            &raw[1..raw.len() - 1],
                        ))));
                    }
                    (SymbolID::VALUE, [N(SymbolID::ARRAY)]) => {
                        let array = pop_stack_item!(Array);
                        stack.push(StackItem::Value(Value::Array(array)));
                    }
                    (SymbolID::VALUE, [N(SymbolID::OBJECT)]) => {
                        let object = pop_stack_item!(Object);
                        stack.push(StackItem::Value(Value::Object(object)));
                    }

                    (SymbolID::OBJECT, [T(Token::LBracket), T(Token::RBracket)]) => {
                        stack.push(StackItem::Object(Object {
                            members: Default::default(),
                        }));
                    }
                    (
                        SymbolID::OBJECT,
                        [T(Token::LBracket), N(SymbolID::MEMBERS), T(Token::RBracket)],
                    ) => {
                        let members = pop_stack_item!(Members);
                        stack.push(StackItem::Object(Object {
                            members: members.into_iter().collect(),
                        }));
                    }

                    (SymbolID::MEMBERS, [N(SymbolID::MEMBER)]) => {
                        let (key, value) = pop_stack_item!(Member);
                        let members = vec![(key, value)];
                        stack.push(StackItem::Members(members));
                    }
                    (
                        SymbolID::MEMBERS,
                        [N(SymbolID::MEMBER), T(Token::Comma), N(SymbolID::MEMBERS)],
                    ) => {
                        let mut members = pop_stack_item!(Members);
                        let (key, value) = pop_stack_item!(Member);
                        members.push((key, value));
                        stack.push(StackItem::Members(members));
                    }

                    (
                        SymbolID::MEMBER,
                        [T(Token::String(key)), T(Token::Colon), N(SymbolID::VALUE)],
                    ) => {
                        let value = pop_stack_item!(Value);
                        stack.push(StackItem::Member((
                            EscapedStr::from_raw(&key[1..key.len() - 1]),
                            Box::new(value),
                        )));
                    }

                    (SymbolID::ARRAY, [T(Token::LBrace), T(Token::RBrace)]) => {
                        stack.push(StackItem::Array(Array { elements: vec![] }));
                    }
                    (
                        SymbolID::ARRAY,
                        [T(Token::LBrace), N(SymbolID::ELEMENTS), T(Token::RBrace)],
                    ) => {
                        let mut elements = pop_stack_item!(Elements);
                        elements.reverse();
                        stack.push(StackItem::Array(Array { elements }));
                    }

                    (SymbolID::ELEMENTS, [N(SymbolID::VALUE)]) => {
                        let elem = pop_stack_item!(Value);
                        stack.push(StackItem::Elements(vec![Box::new(elem)]));
                    }

                    (
                        SymbolID::ELEMENTS,
                        [N(SymbolID::VALUE), T(Token::Comma), N(SymbolID::ELEMENTS)],
                    ) => {
                        let mut elems = pop_stack_item!(Elements);
                        let elem = pop_stack_item!(Value);
                        elems.push(Box::new(elem));
                        stack.push(StackItem::Elements(elems));
                    }

                    _ => unreachable!(),
                }
            }

            ParseEvent::AboutToAccept(..) => {
                tracing::trace!("about to accept");
            }

            ParseEvent::HandlingError {
                lr_state,
                lookahead,
                expected,
            } => {
                tracing::trace!(
                    "handling error: lr_state={:?}, lookahead={:?}, expected={:?}",
                    lr_state,
                    lookahead,
                    expected,
                );

                continue;
            }

            ParseEvent::Accepted => {
                tracing::trace!("accepted");
                match stack.pop() {
                    Some(StackItem::Value(value)) => break value,
                    _ => anyhow::bail!("invalid stack item"),
                }
            }

            ParseEvent::Rejected => {
                tracing::trace!("rejected");
                anyhow::bail!("rejected");
            }
        }
    };

    println!("parsed: {}", parsed);

    Ok(())
}
