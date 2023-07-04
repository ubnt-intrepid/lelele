pub mod ast;
pub mod grammar;
pub mod lexer;

use self::{
    grammar::{ParserDef, Symbol},
    lexer::{Lexer, Token},
};
use lelele_runtime::parser::{ParseEvent, ParseItem::*, Parser};
use StackItem as s;
use Symbol::*;
use Token::*;

enum StackItem {
    Grammar(ast::Grammar),
    Descs(Vec<ast::Desc>),
    Desc(ast::Desc),
    TerminalDesc(ast::TerminalDesc),
    NonterminalDesc(ast::NonterminalDesc),
    RuleDesc(ast::RuleDesc),
    PrecDesc(ast::PrecDesc),
    StartDesc(ast::StartDesc),
    Productions(Vec<ast::Production>),
    Production(ast::Production),
    Elems(Vec<String>),
    Idents(Vec<String>),
    Configs(Vec<ast::Config>),
    Config(ast::Config),
}

pub fn parse(source: &str) -> anyhow::Result<ast::Grammar> {
    let span = tracing::trace_span!("parse");
    let _entered = span.enter();

    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(ParserDef::default());
    let mut stack: Vec<StackItem> = vec![];
    macro_rules! pop_stack {
        ($Variant:ident) => {
            match stack.pop() {
                Some(StackItem::$Variant(item)) => item,
                _ => anyhow::bail!(concat!(
                    "unexpected stack item, expecting ",
                    stringify!($Variant)
                )),
            }
        };
    }
    macro_rules! peek_stack {
        ($Variant:ident) => {
            match stack.last_mut() {
                Some(StackItem::$Variant(item)) => item,
                _ => anyhow::bail!(concat!(
                    "unexpected stack item, expecting ",
                    stringify!($Variant)
                )),
            }
        };
    }

    loop {
        let event = parser.resume()?;
        match event {
            ParseEvent::InputNeeded => match lexer.next().transpose()? {
                Some(tok) => {
                    tracing::trace!("offer token {:?}", tok);
                    parser.offer_token(tok)?;
                }
                None => {
                    tracing::trace!("offer EOI");
                    parser.offer_eoi()?;
                }
            },
            ParseEvent::Shifting(_lookahead) => {
                //tracing::trace!("shifting: {:?}", lookahead);
            }
            ParseEvent::AboutToReduce(lhs, args) => {
                tracing::trace!("reducing: {:?} -> {:?}", lhs, args);
                match (lhs, args) {
                    (Grammar, [N(Descs)]) => {
                        let descs = pop_stack!(Descs);
                        stack.push(s::Grammar(ast::Grammar { descs }));
                    }

                    (Descs, []) => {
                        stack.push(s::Descs(vec![]));
                    }
                    (Descs, [N(Descs), N(Desc), T(Semicolon)]) => {
                        let desc = pop_stack!(Desc);
                        let descs = peek_stack!(Descs);
                        descs.push(desc);
                    }

                    (Desc, [N(TerminalDesc)]) => {
                        let desc = pop_stack!(TerminalDesc);
                        stack.push(s::Desc(ast::Desc::Terminal(desc)));
                    }
                    (Desc, [N(NonterminalDesc)]) => {
                        let desc = pop_stack!(NonterminalDesc);
                        stack.push(s::Desc(ast::Desc::Nonterminal(desc)));
                    }
                    (Desc, [N(RuleDesc)]) => {
                        let desc = pop_stack!(RuleDesc);
                        stack.push(s::Desc(ast::Desc::Rule(desc)));
                    }
                    (Desc, [N(PrecDesc)]) => {
                        let desc = pop_stack!(PrecDesc);
                        stack.push(s::Desc(ast::Desc::Prec(desc)));
                    }
                    (Desc, [N(StartDesc)]) => {
                        let desc = pop_stack!(StartDesc);
                        stack.push(s::Desc(ast::Desc::Start(desc)));
                    }

                    (
                        PrecDesc,
                        [T(Prec), T(LBracket), N(Configs), T(RBracket), T(Ident(ident))],
                    ) => {
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::PrecDesc(ast::PrecDesc {
                            configs,
                            ident: ident.to_string(),
                        }));
                    }

                    (TerminalDesc, [T(Terminal), N(Idents)]) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(s::TerminalDesc(ast::TerminalDesc {
                            configs: vec![],
                            idents,
                        }));
                    }
                    (
                        TerminalDesc,
                        [T(Terminal), T(LBracket), N(Configs), T(RBracket), N(Idents)],
                    ) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::TerminalDesc(ast::TerminalDesc { configs, idents }));
                    }

                    (NonterminalDesc, [T(Nonterminal), N(Idents)]) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(s::NonterminalDesc(ast::NonterminalDesc { idents }));
                    }

                    (StartDesc, [T(Start), T(Ident(name))]) => {
                        stack.push(s::StartDesc(ast::StartDesc {
                            name: name.to_string(),
                        }));
                    }

                    (RuleDesc, [T(Rule), T(Ident(left)), T(ColonEq), N(Productions)]) => {
                        let productions = pop_stack!(Productions);
                        stack.push(s::RuleDesc(ast::RuleDesc {
                            left: left.to_string(),
                            productions,
                        }));
                    }
                    (
                        RuleDesc,
                        [T(Rule), T(Ident(left)), T(ColonEq), T(VertBar), N(Productions)],
                    ) => {
                        let productions = pop_stack!(Productions);
                        stack.push(s::RuleDesc(ast::RuleDesc {
                            left: left.to_string(),
                            productions,
                        }));
                    }

                    (Productions, [N(Production)]) => {
                        let production = pop_stack!(Production);
                        stack.push(s::Productions(vec![production]));
                    }
                    (Productions, [N(Productions), T(VertBar), N(Production)]) => {
                        let production = pop_stack!(Production);
                        let productions = peek_stack!(Productions);
                        productions.push(production);
                    }

                    (Production, [T(Empty)]) => {
                        stack.push(s::Production(ast::Production {
                            configs: vec![],
                            elems: vec![],
                        }));
                    }
                    (Production, [N(Elems)]) => {
                        let mut elems = pop_stack!(Elems);
                        elems.reverse();
                        stack.push(s::Production(ast::Production {
                            configs: vec![],
                            elems,
                        }));
                    }
                    (Production, [T(AtLBracket), N(Configs), T(RBracket), N(Elems)]) => {
                        let mut elems = pop_stack!(Elems);
                        elems.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::Production(ast::Production { configs, elems }));
                    }

                    (Elems, [T(Ident(ident))]) => {
                        stack.push(s::Elems(vec![ident.to_string()]));
                    }
                    (Elems, [T(Ident(elem)), N(Elems)]) => {
                        let elems = peek_stack!(Elems);
                        elems.push(elem.to_string());
                    }

                    (Idents, [T(Ident(ident))]) | (Idents, [T(Ident(ident)), T(Comma)]) => {
                        stack.push(s::Idents(vec![ident.to_string()]));
                    }
                    (Idents, [T(Ident(ident)), T(Comma), N(Idents)]) => {
                        let idents = peek_stack!(Idents);
                        idents.push(ident.to_string());
                    }

                    (Configs, [N(Config)]) | (Configs, [N(Config), T(Comma)]) => {
                        let config = pop_stack!(Config);
                        stack.push(s::Configs(vec![config]));
                    }
                    (Configs, [N(Config), T(Comma), N(Configs)]) => {
                        let mut configs = pop_stack!(Configs);
                        let config = pop_stack!(Config);
                        configs.push(config);
                        stack.push(s::Configs(configs));
                    }

                    (Config, [T(Ident(key)), T(Eq), T(Ident(value))]) => {
                        stack.push(s::Config(ast::Config {
                            key: key.to_string(),
                            value: value.to_string(),
                        }));
                    }

                    _ => unreachable!(),
                }
            }
            ParseEvent::Accepted => {
                tracing::trace!("accepted");
                let grammar = pop_stack!(Grammar);
                tracing::trace!(" --> {:?}", grammar);
                return Ok(grammar);
            }

            ParseEvent::HandlingError {
                lr_state,
                lookahead,
                expected,
            } => {
                tracing::trace!(
                    "handling error: {:?}, {:?}, {:?}",
                    lr_state,
                    lookahead,
                    expected
                );
                anyhow::bail!("syntax error: expecting {:?}", expected);
            }

            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tracing::Level;

    #[test]
    fn smoketest() {
        tracing_subscriber::fmt()
            .with_ansi(false)
            .with_max_level(Level::TRACE)
            .init();

        let input = "\
@terminal A, B, C;
@nonterminal E;
@rule E := A B C;
@rule E :=
      @empty
    | B C A
    | E D
    ;
";
        let _parsed = parse(input).unwrap();
    }
}
