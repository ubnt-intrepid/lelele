use super::{
    grammar::{ParserDef, Symbol},
    lexer::{Keyword, Lexer, Token},
};
use lelele_runtime::engine::{ParseEngine, ParseEvent, Symbol::*};
use std::cmp::Ordering;

#[derive(Debug)]
pub struct File {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    TerminalDesc(TerminalDesc),
    NonterminalDesc(NonterminalDesc),
    RuleDesc(RuleDesc),
    PrecDesc(PrecDesc),
    StartDesc(StartDesc),
}
impl Stmt {
    pub fn cmp_by_desc(&self, other: &Self) -> Ordering {
        use Ordering::*;
        match self {
            Stmt::PrecDesc(..) => match other {
                Stmt::PrecDesc(..) => Equal,
                _ => Less,
            },
            Stmt::TerminalDesc(..) => match other {
                Stmt::PrecDesc(..) => Greater,
                Stmt::TerminalDesc(..) => Equal,
                _ => Less,
            },
            Stmt::NonterminalDesc(..) => match other {
                Stmt::RuleDesc(..) | Stmt::StartDesc(..) => Less,
                Stmt::NonterminalDesc(..) => Equal,
                Stmt::TerminalDesc(..) | Stmt::PrecDesc(..) => Greater,
            },
            Stmt::RuleDesc(..) => match other {
                Stmt::StartDesc(..) => Less,
                Stmt::RuleDesc(..) => Equal,
                _ => Greater,
            },
            Stmt::StartDesc(..) => match other {
                Stmt::StartDesc(..) => Equal,
                _ => Greater,
            },
        }
    }
}

#[derive(Debug)]
pub struct TerminalDesc {
    pub configs: Vec<Config>,
    pub idents: Vec<String>,
}

#[derive(Debug)]
pub struct NonterminalDesc {
    pub idents: Vec<String>,
}

#[derive(Debug)]
pub struct RuleDesc {
    pub left: String,
    pub productions: Vec<Production>,
}

#[derive(Debug)]
pub struct PrecDesc {
    pub configs: Vec<Config>,
    pub ident: String,
}

#[derive(Debug)]
pub struct StartDesc {
    pub name: String,
}

#[derive(Debug)]
pub struct Production {
    pub configs: Vec<Config>,
    pub elems: Vec<ProductionElem>,
}

#[derive(Debug)]
pub enum ProductionElem {
    Ident(String),
    ErrorToken,
}

#[derive(Debug)]
pub struct Config {
    pub key: String,
    pub value: String,
}

enum StackItem {
    Grammar(File),
    Descs(Vec<Stmt>),
    Desc(Stmt),
    Productions(Vec<Production>),
    Production(Production),
    ProductionElems(Vec<ProductionElem>),
    ProductionElem(ProductionElem),
    Idents(Vec<String>),
    Configs(Vec<Config>),
    Config(Config),
}

pub fn parse(source: &str) -> anyhow::Result<File> {
    let span = tracing::trace_span!("parse");
    let _entered = span.enter();

    let mut lexer = Lexer::new(source);
    let mut engine = ParseEngine::new(ParserDef::default());
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
        let event = engine.resume()?;
        match event {
            ParseEvent::InputNeeded => {
                let token = lexer
                    .next()
                    .transpose()
                    .map_err(|_e| anyhow::anyhow!("lexer error"))?;
                match token {
                    Some(tok) => {
                        tracing::trace!("offer token {:?}", tok);
                        engine.offer_token(tok)?;
                    }
                    None => {
                        tracing::trace!("offer EOI");
                        engine.offer_eoi()?;
                    }
                }
            }
            ParseEvent::Shifting(_lookahead) => {
                //tracing::trace!("shifting: {:?}", lookahead);
            }
            ParseEvent::AboutToReduce(lhs, args) => {
                tracing::trace!("reducing: {:?} -> {:?}", lhs, args);
                match (lhs, args) {
                    (Symbol::Grammar, [N(Symbol::Stmts)]) => {
                        let descs = pop_stack!(Descs);
                        stack.push(StackItem::Grammar(File { stmts: descs }));
                    }

                    (Symbol::Stmts, []) => {
                        stack.push(StackItem::Descs(vec![]));
                    }
                    (
                        Symbol::Stmts,
                        [N(Symbol::Stmts), N(Symbol::Stmt), T((_, Token::Semicolon, _))],
                    ) => {
                        let desc = pop_stack!(Desc);
                        let descs = peek_stack!(Descs);
                        descs.push(desc);
                    }

                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Terminal), _)), N(Symbol::Idents)],
                    ) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(StackItem::Desc(Stmt::TerminalDesc(TerminalDesc {
                            configs: vec![],
                            idents,
                        })));
                    }
                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Terminal), _)), T((_, Token::LBracket, _)), N(Symbol::Configs), T((_, Token::RBracket, _)), N(Symbol::Idents)],
                    ) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(StackItem::Desc(Stmt::TerminalDesc(TerminalDesc {
                            configs,
                            idents,
                        })));
                    }

                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Nonterminal), _)), N(Symbol::Idents)],
                    ) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(StackItem::Desc(Stmt::NonterminalDesc(NonterminalDesc {
                            idents,
                        })));
                    }

                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Rule), _)), T((_, Token::Ident(left), _)), T((_, Token::ColonEq, _)), N(Symbol::Productions)],
                    ) => {
                        let productions = pop_stack!(Productions);
                        stack.push(StackItem::Desc(Stmt::RuleDesc(RuleDesc {
                            left: left.to_string(),
                            productions,
                        })));
                    }
                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Rule), _)), T((_, Token::Ident(left), _)), T((_, Token::ColonEq, _)), T((_, Token::VertBar, _)), N(Symbol::Productions)],
                    ) => {
                        let productions = pop_stack!(Productions);
                        stack.push(StackItem::Desc(Stmt::RuleDesc(RuleDesc {
                            left: left.to_string(),
                            productions,
                        })));
                    }

                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Start), _)), T((_, Token::Ident(name), _))],
                    ) => {
                        stack.push(StackItem::Desc(Stmt::StartDesc(StartDesc {
                            name: name.to_string(),
                        })));
                    }

                    (
                        Symbol::Stmt,
                        [T((_, Token::Kw(Keyword::Prec), _)), T((_, Token::LBracket, _)), N(Symbol::Configs), T((_, Token::RBracket, _)), T((_, Token::Ident(ident), _))],
                    ) => {
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(StackItem::Desc(Stmt::PrecDesc(PrecDesc {
                            configs,
                            ident: ident.to_string(),
                        })));
                    }

                    (Symbol::Productions, [N(Symbol::Production)]) => {
                        let production = pop_stack!(Production);
                        stack.push(StackItem::Productions(vec![production]));
                    }
                    (
                        Symbol::Productions,
                        [N(Symbol::Productions), T((_, Token::VertBar, _)), N(Symbol::Production)],
                    ) => {
                        let production = pop_stack!(Production);
                        let productions = peek_stack!(Productions);
                        productions.push(production);
                    }

                    (Symbol::Production, [T((_, Token::Kw(Keyword::Empty), _))]) => {
                        stack.push(StackItem::Production(Production {
                            configs: vec![],
                            elems: vec![],
                        }));
                    }
                    (Symbol::Production, [N(Symbol::ProductionElems)]) => {
                        let mut elems = pop_stack!(ProductionElems);
                        elems.reverse();
                        stack.push(StackItem::Production(Production {
                            configs: vec![],
                            elems,
                        }));
                    }
                    (
                        Symbol::Production,
                        [T((_, Token::AtLBracket, _)), N(Symbol::Configs), T((_, Token::RBracket, _)), N(Symbol::ProductionElems)],
                    ) => {
                        let mut elems = pop_stack!(ProductionElems);
                        elems.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(StackItem::Production(Production { configs, elems }));
                    }

                    (Symbol::ProductionElems, [N(Symbol::ProductionElem)]) => {
                        let elem = pop_stack!(ProductionElem);
                        stack.push(StackItem::ProductionElems(vec![elem]));
                    }
                    (
                        Symbol::ProductionElems,
                        [N(Symbol::ProductionElem), N(Symbol::ProductionElems)],
                    ) => {
                        let mut elems = pop_stack!(ProductionElems);
                        let elem = pop_stack!(ProductionElem);
                        elems.push(elem);
                        stack.push(StackItem::ProductionElems(elems));
                    }

                    (Symbol::ProductionElem, [T((_, Token::Ident(ident), _))]) => {
                        stack.push(StackItem::ProductionElem(ProductionElem::Ident(
                            ident.to_string(),
                        )));
                    }
                    (Symbol::ProductionElem, [T((_, Token::Kw(Keyword::Error), _))]) => {
                        stack.push(StackItem::ProductionElem(ProductionElem::ErrorToken));
                    }

                    (Symbol::Idents, [T((_, Token::Ident(ident), _))]) => {
                        stack.push(StackItem::Idents(vec![ident.to_string()]));
                    }
                    (Symbol::Idents, [T((_, Token::Ident(ident), _)), N(Symbol::Idents)]) => {
                        let idents = peek_stack!(Idents);
                        idents.push(ident.to_string());
                    }

                    (Symbol::Configs, [N(Symbol::Config)])
                    | (Symbol::Configs, [N(Symbol::Config), T((_, Token::Comma, _))]) => {
                        let config = pop_stack!(Config);
                        stack.push(StackItem::Configs(vec![config]));
                    }
                    (
                        Symbol::Configs,
                        [N(Symbol::Config), T((_, Token::Comma, _)), N(Symbol::Configs)],
                    ) => {
                        let mut configs = pop_stack!(Configs);
                        let config = pop_stack!(Config);
                        configs.push(config);
                        stack.push(StackItem::Configs(configs));
                    }

                    (
                        Symbol::Config,
                        [T((_, Token::Ident(key), _)), T((_, Token::Eq, _)), T((_, Token::Ident(value), _))],
                    ) => {
                        stack.push(StackItem::Config(Config {
                            key: key.to_string(),
                            value: value.to_string(),
                        }));
                    }

                    _ => unreachable!(),
                }
            }
            ParseEvent::Accepted(recovered) => {
                tracing::trace!("accepted");
                if recovered > 0 {
                    tracing::trace!("accepted: recover {} token(s)", recovered);
                }
                let grammar = pop_stack!(Grammar);
                tracing::trace!(" --> {:?}", grammar);
                return Ok(grammar);
            }

            ParseEvent::HandlingError {
                state,
                lookahead,
                expected,
            } => {
                tracing::trace!(
                    "handling error: state={:?}, lookahead={:?}, expected={:?}",
                    state,
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
    use tracing_subscriber::EnvFilter;

    #[test]
    fn smoketest() {
        tracing_subscriber::fmt()
            .with_ansi(false)
            .with_test_writer()
            .with_env_filter(EnvFilter::from_default_env())
            .init();

        let input = "\
@terminal A B C;
@nonterminal D E;
@rule E := A B C;
@rule E :=
    | @empty
    | B C A
    | E D
    ;
@rule D :=
    | A
    | @error
    ;
";
        let _parsed = parse(input).unwrap();
    }
}
