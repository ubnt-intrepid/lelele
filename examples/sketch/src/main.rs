use anyhow::Context;
use lelele_example_sketch::{
    lexer::{lexer, Token},
    parser::{ParserDef, SymbolID},
    syntax::Expr,
};
use lelele_runtime::parser::{ParseEvent, ParseItem, Parser};
use std::env;
use ParseItem::*;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(tracing::Level::TRACE)
        .init();

    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = Parser::new(ParserDef::default());
    let mut ast_stack = vec![];
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

            ParseEvent::AboutToReduce(
                SymbolID::EXPR,
                [N(SymbolID::EXPR), T(op), N(SymbolID::EXPR)],
            ) => {
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

            ParseEvent::AboutToReduce(SymbolID::EXPR, [T(Token::Num(num))]) => {
                tracing::trace!("reduce: expr -> NUM");
                ast_stack.push(Box::new(Expr::Num(num)));
            }

            ParseEvent::AboutToReduce(
                SymbolID::EXPR,
                [T(l_paren @ Token::LParen), N(SymbolID::EXPR), T(r_paren @ Token::RParen)],
            ) => {
                tracing::trace!("reduce: expr -> `(' expr `)'");
                let expr = ast_stack.pop().context("unexpected stack item")?;
                ast_stack.push(Box::new(Expr::Paren {
                    l_paren: *l_paren,
                    expr,
                    r_paren: *r_paren,
                }));
            }
            ParseEvent::AboutToReduce(
                SymbolID::EXPR,
                [T(minus @ Token::Minus), N(SymbolID::EXPR)],
            ) => {
                tracing::trace!("reduce: expr -> `-' expr");
                let expr = ast_stack.pop().context("unexpected stack item")?;
                ast_stack.push(Box::new(Expr::Neg {
                    minus: *minus,
                    expr,
                }));
            }

            ParseEvent::AboutToReduce(..) => unreachable!(),

            ParseEvent::AboutToAccept(..) => {
                tracing::trace!("about to accept");
                match ast_stack.pop() {
                    Some(expr) => {
                        ast_stack.push(expr);
                    }
                    _ => anyhow::bail!("unexpected stack item"),
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
                let parsed = ast_stack.pop().context("unexpected stack item")?;
                break parsed;
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
