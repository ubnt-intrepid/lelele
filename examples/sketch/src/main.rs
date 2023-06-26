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
    let mut ast_stack = vec![];
    loop {
        match parser.resume().map_err(|e| {
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
            ParseEvent::Shifting(_lookahead) => {}
            ParseEvent::AboutToReduce(RuleID::EXPR_ADD, args) => {
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Add { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_SUB, args) => {
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Sub { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_MUL, args) => {
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Mul { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_DIV, args) => {
                match (ast_stack.pop(), args[1], ast_stack.pop()) {
                    (Some(rhs), T(Some(op)), Some(lhs)) => {
                        ast_stack.push(Box::new(Expr::Div { lhs, op, rhs }))
                    }
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::AboutToReduce(RuleID::EXPR_NUM, args) => match args[0] {
                T(Some(Token::Num(num))) => {
                    ast_stack.push(Box::new(Expr::Num(num)));
                }
                _ => anyhow::bail!("unexpected parse item"),
            },
            ParseEvent::AboutToReduce(RuleID::EXPR_PAREN, args) => {
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
            ParseEvent::AboutToReduce(RuleID::EXPR_NEG, args) => match (args[0], ast_stack.pop()) {
                (T(Some(minus)), Some(expr)) => {
                    ast_stack.push(Box::new(Expr::Neg { minus, expr }));
                }
                _ => anyhow::bail!("unexpected parse/stack item"),
            },

            ParseEvent::AboutToAccept(..) => {
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
