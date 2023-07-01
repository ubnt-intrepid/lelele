mod gen {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}
pub use gen::*;

use crate::{
    lexer::Token,
    syntax::{Array, EscapedStr, Object, Value},
};
use lelele_runtime::parser::{ParseEvent, ParseItem::*, Parser};

enum StackItem<'source> {
    Value(Value<'source>),
    Object(Object<'source>),
    Array(Array<'source>),
    Member((&'source EscapedStr, Box<Value<'source>>)),
    Members(Vec<(&'source EscapedStr, Box<Value<'source>>)>),
    Elements(Vec<Box<Value<'source>>>),
}

pub fn parse(input: &str) -> anyhow::Result<Value<'_>> {
    let mut lexer = crate::lexer::lexer(&input);
    let mut parser = Parser::new(ParserDef::default());
    let mut stack = vec![];
    macro_rules! pop_stack_item {
        ($variant:ident) => {{
            match stack.pop() {
                Some(StackItem::$variant(v)) => v,
                _ => anyhow::bail!("unexpected stack item"),
            }
        }};
    }

    loop {
        let span = tracing::trace_span!("resume");
        let _entered = span.enter();

        let event = parser.resume().map_err(|e| {
            eprintln!("parse error: {:?}", e);
            e
        })?;
        match event {
            ParseEvent::InputNeeded => match lexer.next() {
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
                    (Symbol::Value, [T(Token::Null)]) => {
                        stack.push(StackItem::Value(Value::Null));
                    }
                    (Symbol::Value, [T(Token::True)]) => {
                        stack.push(StackItem::Value(Value::Bool(true)));
                    }
                    (Symbol::Value, [T(Token::False)]) => {
                        stack.push(StackItem::Value(Value::Bool(false)));
                    }
                    (Symbol::Value, [T(Token::Number(number))]) => {
                        stack.push(StackItem::Value(Value::Number(number)));
                    }
                    (Symbol::Value, [T(Token::String(raw))]) => {
                        stack.push(StackItem::Value(Value::String(EscapedStr::from_raw(
                            &raw[1..raw.len() - 1],
                        ))));
                    }
                    (Symbol::Value, [N(Symbol::Array)]) => {
                        let array = pop_stack_item!(Array);
                        stack.push(StackItem::Value(Value::Array(array)));
                    }
                    (Symbol::Value, [N(Symbol::Object)]) => {
                        let object = pop_stack_item!(Object);
                        stack.push(StackItem::Value(Value::Object(object)));
                    }

                    (Symbol::Object, [T(Token::LBracket), T(Token::RBracket)]) => {
                        stack.push(StackItem::Object(Object {
                            members: Default::default(),
                        }));
                    }
                    (
                        Symbol::Object,
                        [T(Token::LBracket), N(Symbol::Members), T(Token::RBracket)],
                    ) => {
                        let members = pop_stack_item!(Members);
                        stack.push(StackItem::Object(Object {
                            members: members.into_iter().collect(),
                        }));
                    }

                    (Symbol::Members, [N(Symbol::Member)]) => {
                        let (key, value) = pop_stack_item!(Member);
                        let members = vec![(key, value)];
                        stack.push(StackItem::Members(members));
                    }
                    (Symbol::Members, [N(Symbol::Member), T(Token::Comma), N(Symbol::Members)]) => {
                        let mut members = pop_stack_item!(Members);
                        let (key, value) = pop_stack_item!(Member);
                        members.push((key, value));
                        stack.push(StackItem::Members(members));
                    }

                    (
                        Symbol::Member,
                        [T(Token::String(key)), T(Token::Colon), N(Symbol::Value)],
                    ) => {
                        let value = pop_stack_item!(Value);
                        stack.push(StackItem::Member((
                            EscapedStr::from_raw(&key[1..key.len() - 1]),
                            Box::new(value),
                        )));
                    }

                    (Symbol::Array, [T(Token::LBrace), T(Token::RBrace)]) => {
                        stack.push(StackItem::Array(Array { elements: vec![] }));
                    }
                    (Symbol::Array, [T(Token::LBrace), N(Symbol::Elements), T(Token::RBrace)]) => {
                        let mut elements = pop_stack_item!(Elements);
                        elements.reverse();
                        stack.push(StackItem::Array(Array { elements }));
                    }

                    (Symbol::Elements, [N(Symbol::Value)]) => {
                        let elem = pop_stack_item!(Value);
                        stack.push(StackItem::Elements(vec![Box::new(elem)]));
                    }

                    (
                        Symbol::Elements,
                        [N(Symbol::Value), T(Token::Comma), N(Symbol::Elements)],
                    ) => {
                        let mut elems = pop_stack_item!(Elements);
                        let elem = pop_stack_item!(Value);
                        elems.push(Box::new(elem));
                        stack.push(StackItem::Elements(elems));
                    }

                    _ => unreachable!(),
                }
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
                    Some(StackItem::Value(value)) => break Ok(value),
                    _ => anyhow::bail!("invalid stack item"),
                }
            }

            ParseEvent::Rejected => {
                tracing::trace!("rejected");
                anyhow::bail!("rejected");
            }
        }
    }
}
