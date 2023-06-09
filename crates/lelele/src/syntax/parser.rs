use super::{
    grammar::{ParserDef, Symbol},
    lexer::{Keyword, Lexer, Token},
};
use lelele_runtime::engine::{ParseEngine, ParseEvent, Symbol::*};
use Keyword::*;
use StackItem as s;
use Symbol::*;
use Token::*;

enum StackItem {
    Grammar(super::Grammar),
    Descs(Vec<super::Stmt>),
    Desc(super::Stmt),
    Productions(Vec<super::Production>),
    Production(super::Production),
    ProductionElems(Vec<super::ProductionElem>),
    ProductionElem(super::ProductionElem),
    Idents(Vec<String>),
    Configs(Vec<super::Config>),
    Config(super::Config),
}

pub fn parse(source: &str) -> anyhow::Result<super::Grammar> {
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
                    (Grammar, [N(Stmts)]) => {
                        let descs = pop_stack!(Descs);
                        stack.push(s::Grammar(super::Grammar { stmts: descs }));
                    }

                    (Stmts, []) => {
                        stack.push(s::Descs(vec![]));
                    }
                    (Stmts, [N(Stmts), N(Stmt), T((_, Semicolon, _))]) => {
                        let desc = pop_stack!(Desc);
                        let descs = peek_stack!(Descs);
                        descs.push(desc);
                    }

                    (Stmt, [T((_, Kw(Terminal), _)), N(Idents)]) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(s::Desc(super::Stmt::TerminalDesc(super::TerminalDesc {
                            configs: vec![],
                            idents,
                        })));
                    }
                    (
                        Stmt,
                        [T((_, Kw(Terminal), _)), T((_, LBracket, _)), N(Configs), T((_, RBracket, _)), N(Idents)],
                    ) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::Desc(super::Stmt::TerminalDesc(super::TerminalDesc {
                            configs,
                            idents,
                        })));
                    }

                    (Stmt, [T((_, Kw(Nonterminal), _)), N(Idents)]) => {
                        let mut idents = pop_stack!(Idents);
                        idents.reverse();
                        stack.push(s::Desc(super::Stmt::NonterminalDesc(
                            super::NonterminalDesc { idents },
                        )));
                    }

                    (
                        Stmt,
                        [T((_, Kw(Rule), _)), T((_, Ident(left), _)), T((_, ColonEq, _)), N(Productions)],
                    ) => {
                        let productions = pop_stack!(Productions);
                        stack.push(s::Desc(super::Stmt::RuleDesc(super::RuleDesc {
                            left: left.to_string(),
                            productions,
                        })));
                    }
                    (
                        Stmt,
                        [T((_, Kw(Rule), _)), T((_, Ident(left), _)), T((_, ColonEq, _)), T((_, VertBar, _)), N(Productions)],
                    ) => {
                        let productions = pop_stack!(Productions);
                        stack.push(s::Desc(super::Stmt::RuleDesc(super::RuleDesc {
                            left: left.to_string(),
                            productions,
                        })));
                    }

                    (Stmt, [T((_, Kw(Start), _)), T((_, Ident(name), _))]) => {
                        stack.push(s::Desc(super::Stmt::StartDesc(super::StartDesc {
                            name: name.to_string(),
                        })));
                    }

                    (
                        Stmt,
                        [T((_, Kw(Prec), _)), T((_, LBracket, _)), N(Configs), T((_, RBracket, _)), T((_, Ident(ident), _))],
                    ) => {
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::Desc(super::Stmt::PrecDesc(super::PrecDesc {
                            configs,
                            ident: ident.to_string(),
                        })));
                    }

                    (Productions, [N(Production)]) => {
                        let production = pop_stack!(Production);
                        stack.push(s::Productions(vec![production]));
                    }
                    (Productions, [N(Productions), T((_, VertBar, _)), N(Production)]) => {
                        let production = pop_stack!(Production);
                        let productions = peek_stack!(Productions);
                        productions.push(production);
                    }

                    (Production, [T((_, Kw(Empty), _))]) => {
                        stack.push(s::Production(super::Production {
                            configs: vec![],
                            elems: vec![],
                        }));
                    }
                    (Production, [N(ProductionElems)]) => {
                        let mut elems = pop_stack!(ProductionElems);
                        elems.reverse();
                        stack.push(s::Production(super::Production {
                            configs: vec![],
                            elems,
                        }));
                    }
                    (
                        Production,
                        [T((_, AtLBracket, _)), N(Configs), T((_, RBracket, _)), N(ProductionElems)],
                    ) => {
                        let mut elems = pop_stack!(ProductionElems);
                        elems.reverse();
                        let mut configs = pop_stack!(Configs);
                        configs.reverse();
                        stack.push(s::Production(super::Production { configs, elems }));
                    }

                    (ProductionElems, [N(ProductionElem)]) => {
                        let elem = pop_stack!(ProductionElem);
                        stack.push(s::ProductionElems(vec![elem]));
                    }
                    (ProductionElems, [N(ProductionElem), N(ProductionElems)]) => {
                        let mut elems = pop_stack!(ProductionElems);
                        let elem = pop_stack!(ProductionElem);
                        elems.push(elem);
                        stack.push(s::ProductionElems(elems));
                    }

                    (ProductionElem, [T((_, Ident(ident), _))]) => {
                        stack.push(s::ProductionElem(super::ProductionElem::Ident(
                            ident.to_string(),
                        )));
                    }
                    (ProductionElem, [T((_, Kw(Keyword::Error), _))]) => {
                        stack.push(s::ProductionElem(super::ProductionElem::ErrorToken));
                    }

                    (Idents, [T((_, Ident(ident), _))]) => {
                        stack.push(s::Idents(vec![ident.to_string()]));
                    }
                    (Idents, [T((_, Ident(ident), _)), N(Idents)]) => {
                        let idents = peek_stack!(Idents);
                        idents.push(ident.to_string());
                    }

                    (Configs, [N(Config)]) | (Configs, [N(Config), T((_, Comma, _))]) => {
                        let config = pop_stack!(Config);
                        stack.push(s::Configs(vec![config]));
                    }
                    (Configs, [N(Config), T((_, Comma, _)), N(Configs)]) => {
                        let mut configs = pop_stack!(Configs);
                        let config = pop_stack!(Config);
                        configs.push(config);
                        stack.push(s::Configs(configs));
                    }

                    (Config, [T((_, Ident(key), _)), T((_, Eq, _)), T((_, Ident(value), _))]) => {
                        stack.push(s::Config(super::Config {
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
