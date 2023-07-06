mod gen {
    include!(concat!(env!("OUT_DIR"), "/arithmetic.rs"));
}
pub use gen::*;

use crate::{lexer::Token, syntax::Expr};
use anyhow::Context as _;
use lelele_runtime::engine::{ParseEngine, ParseEvent, Symbol::*};

pub fn parse(input: &str) -> anyhow::Result<Box<Expr<'_>>> {
    let span = tracing::trace_span!("parse");
    let _entered = span.enter();

    let mut lexer = crate::lexer::lexer(input);
    let mut engine = ParseEngine::new(ParserDef::default());
    let mut ast_stack = vec![];

    loop {
        let event = engine.resume()?;
        match event {
            ParseEvent::InputNeeded => match lexer.next() {
                Some(tok) => {
                    let tok = tok?;
                    tracing::trace!("offer token {:?}", tok);
                    engine.offer_token(tok)?;
                }
                None => {
                    tracing::trace!("offer end of input");
                    engine.offer_eoi()?;
                }
            },

            ParseEvent::Shifting(lookahead) => {
                tracing::trace!("shift: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(Symbol::Expr, [N(Symbol::Expr), T(op), N(Symbol::Expr)]) => {
                let (lhs, rhs) = match (ast_stack.pop(), ast_stack.pop()) {
                    (Some(rhs), Some(lhs)) => (lhs, rhs),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                };
                let expr = match op {
                    Token::Plus => {
                        tracing::trace!("reduce: expr -> expr `+' expr");
                        Expr::Add { lhs, op: *op, rhs }
                    }
                    Token::Minus => {
                        tracing::trace!("reduce: expr -> expr `-' expr");
                        Expr::Sub { lhs, op: *op, rhs }
                    }
                    Token::Star => {
                        tracing::trace!("reduce: expr -> expr `*' expr");
                        Expr::Mul { lhs, op: *op, rhs }
                    }
                    Token::Slash => {
                        tracing::trace!("reduce: expr -> expr `/' expr");
                        Expr::Div { lhs, op: *op, rhs }
                    }
                    _ => unreachable!(),
                };
                ast_stack.push(Box::new(expr));
            }

            ParseEvent::AboutToReduce(Symbol::Expr, [T(Token::Num(num))]) => {
                tracing::trace!("reduce: expr -> NUM");
                ast_stack.push(Box::new(Expr::Num(num)));
            }

            ParseEvent::AboutToReduce(
                Symbol::Expr,
                [T(l_paren @ Token::LParen), N(Symbol::Expr), T(r_paren @ Token::RParen)],
            ) => {
                tracing::trace!("reduce: expr -> `(' expr `)'");
                let expr = ast_stack.pop().context("unexpected stack item")?;
                ast_stack.push(Box::new(Expr::Paren {
                    l_paren: *l_paren,
                    expr,
                    r_paren: *r_paren,
                }));
            }
            ParseEvent::AboutToReduce(Symbol::Expr, [T(minus @ Token::Minus), N(Symbol::Expr)]) => {
                tracing::trace!("reduce: expr -> `-' expr");
                let expr = ast_stack.pop().context("unexpected stack item")?;
                ast_stack.push(Box::new(Expr::Neg {
                    minus: *minus,
                    expr,
                }));
            }

            ParseEvent::AboutToReduce(Symbol::Expr, [ErrorToken]) => {
                tracing::trace!("reduce: expr -> @error");
                ast_stack.push(Box::new(Expr::Error));
            }

            ParseEvent::AboutToReduce(..) => unreachable!(),

            ParseEvent::HandlingError {
                state,
                lookahead,
                expected,
            } => {
                tracing::error!(
                    "caught syntax error: state={:?}, lookahead={:?}, expected={:?}",
                    state,
                    lookahead,
                    expected,
                );
                continue; //
            }

            ParseEvent::Accepted(num_recovered) => {
                tracing::trace!("accepted: recovered = {}", num_recovered);
                let parsed = ast_stack.pop().context("unexpected stack item")?;
                break Ok(parsed);
            }

            ParseEvent::Rejected => {
                tracing::error!("rejected");
                anyhow::bail!("syntax error");
            }
        }
    }
}
