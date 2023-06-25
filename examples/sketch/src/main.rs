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
    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = parser();
    let mut args = vec![];
    let mut ast_stack = vec![];
    loop {
        match parser.next_event(&mut args).map_err(|e| {
            eprintln!("parse error: {:?}", e);
            e
        })? {
            ParseEvent::InputNeeded => match tokens.next() {
                Some(tok) => {
                    parser.offer_token(tok?);
                }
                None => {
                    parser.offer_eoi();
                }
            },
            ParseEvent::AboutToReduce(RuleID::EXPR_ADD) => {
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (Some(rhs), Some(T(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Add { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_SUB) => {
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (Some(rhs), Some(T(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Sub { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_MUL) => {
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (Some(rhs), Some(T(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Mul { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_DIV) => {
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (Some(rhs), Some(T(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Div { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_NUM) => match args[0].take() {
                Some(T(Token::Num(num))) => {
                    ast_stack.push(Box::new(Expr::Num(num)));
                }
                _ => anyhow::bail!("unexpected parse item"),
            },
            ParseEvent::AboutToReduce(RuleID::EXPR_PAREN) => {
                match (args[0].take(), ast_stack.pop(), args[2].take()) {
                    (Some(T(l_paren)), Some(expr), Some(T(r_paren))) => {
                        ast_stack.push(Box::new(Expr::Paren {
                            l_paren,
                            expr,
                            r_paren,
                        }));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_NEG) => {
                match (args[0].take(), ast_stack.pop()) {
                    (Some(T(minus)), Some(expr)) => {
                        ast_stack.push(Box::new(Expr::Neg { minus, expr }));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
            }

            ParseEvent::Accepted => {
                // $accept: expr
                let result = ast_stack.pop().context("unexpected stack item")?;
                println!("parsed: {}", result);
                break;
            }

            _ => unreachable!(),
        }
    }
    Ok(())
}
