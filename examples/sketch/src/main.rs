use anyhow::Context;
use lelele_example_sketch::{
    lexer::lexer,
    parser::{parser, RuleID},
    syntax::{Expr, Factor, Term},
};
use lelele_runtime::parser::{ParseEvent, ParseItem};
use std::env;

enum StackItem<'t> {
    Expr(Box<Expr<'t>>),
    Term(Box<Term<'t>>),
    Factor(Box<Factor<'t>>),
}

fn main() -> anyhow::Result<()> {
    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = parser();
    let mut args = vec![];
    let mut ast_stack = vec![];
    loop {
        match parser.next_event(&mut tokens, &mut args)? {
            ParseEvent::Reduce(RuleID::EXPR_ADD) => {
                // expr : expr '+' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Term(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Expr(Box::new(Expr::Add { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::EXPR_SUB) => {
                // expr : expr '-' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Term(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Expr(Box::new(Expr::Sub { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::EXPR_TERM) => {
                // expr : term
                match ast_stack.pop() {
                    Some(StackItem::Term(term)) => {
                        ast_stack.push(StackItem::Expr(Box::new(Expr::Term(term))));
                    }
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }

            ParseEvent::Reduce(RuleID::TERM_MUL) => {
                // term : term '*' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Factor(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Term(lhs)),
                    ) => ast_stack.push(StackItem::Term(Box::new(Term::Mul { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::TERM_DIV) => {
                // term : term '/' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Factor(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Term(lhs)),
                    ) => ast_stack.push(StackItem::Term(Box::new(Term::Div { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::TERM_FACTOR) => {
                // term : factor
                match ast_stack.pop() {
                    Some(StackItem::Factor(term)) => {
                        ast_stack.push(StackItem::Term(Box::new(Term::Factor(term))));
                    }
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }

            ParseEvent::Reduce(RuleID::FACTOR_NUM) => {
                // factor : num
                match args[0].take() {
                    Some(ParseItem::T(num)) => {
                        ast_stack.push(StackItem::Factor(Box::new(Factor::Num(num))));
                    }
                    _ => anyhow::bail!("unexpected parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::FACTOR_PAREN) => {
                // factor : '(' expr ')'
                match (args[0].take(), ast_stack.pop(), args[2].take()) {
                    (
                        Some(ParseItem::T(l_paren)),
                        Some(StackItem::Expr(expr)),
                        Some(ParseItem::T(r_paren)),
                    ) => {
                        ast_stack.push(StackItem::Factor(Box::new(Factor::Paren {
                            l_paren,
                            expr,
                            r_paren,
                        })));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
            }

            ParseEvent::Accept => {
                // $accept: expr
                let result = match ast_stack.pop() {
                    Some(StackItem::Expr(expr)) => expr,
                    _ => anyhow::bail!("unexpected stack item"),
                };
                println!("parsed: {:?}", result);
                break;
            }

            _ => unreachable!(),
        }
    }
    Ok(())
}
