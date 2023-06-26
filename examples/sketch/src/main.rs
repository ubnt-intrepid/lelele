use anyhow::Context;
use lelele_example_sketch::{
    lexer::{lexer, Token},
    parser::{parser, RuleID},
    syntax::Expr,
};
use lelele_runtime::parser::{ParseEvent, ParseItem};
use std::env;
use ParseItem::*;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(tracing::Level::TRACE)
        .init();

    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = parser();
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
                    parser.offer_token(tok);
                }
                None => {
                    tracing::trace!("offer end of input");
                    parser.offer_eoi();
                }
            },

            ParseEvent::Shifting(lookahead) => {
                tracing::trace!("shifting: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(RuleID::EXPR_ADD, args) => {
                tracing::trace!("about to reduce: expr ::= expr `+' expr");
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Add { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_SUB, args) => {
                tracing::trace!("about to reduce: expr ::= expr `-' expr");
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Sub { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_MUL, args) => {
                tracing::trace!("about to reduce: expr ::= expr `*' expr");
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Mul { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_DIV, args) => {
                tracing::trace!("about to reduce: expr ::= expr `/' expr");
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Div { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_NUM, args) => {
                tracing::trace!("about to reduce: expr ::= NUM");
                match args[0] {
                    T(Some(Token::Num(num))) => {
                        ast_stack.push(Box::new(Expr::Num(num)));
                    }
                    _ => anyhow::bail!("unexpected parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_PAREN, args) => {
                tracing::trace!("about to reduce: expr ::= `(' expr `)'");
                match (args[0], ast_stack.pop(), args[2]) {
                    (T(Some(l_paren)), Some(expr), T(Some(r_paren))) => {
                        ast_stack.push(Box::new(Expr::Paren {
                            l_paren,
                            expr,
                            r_paren,
                        }));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_NEG, args) => {
                tracing::trace!("about to reduce: expr ::= `-' expr");
                match (args[0], ast_stack.pop()) {
                    (T(Some(minus)), Some(expr)) => {
                        ast_stack.push(Box::new(Expr::Neg { minus, expr }));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
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

            ParseEvent::Accepted => {
                tracing::trace!("accepted");
                let parsed = ast_stack.pop().context("unexpected stack item")?;
                break parsed;
            }
        }
    };

    println!("parsed: {}", parsed);

    Ok(())
}
