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
    Factor(Box<Factor<'t>>),
    Term(Box<Term<'t>>),
}

fn main() -> anyhow::Result<()> {
    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = parser();
    let mut args = vec![];
    let mut ast_stack = vec![];
    loop {
        match parser.next_event(&mut tokens, &mut args)? {
            ParseEvent::Reduce(RuleID::EXPR_1) => {
                // expr : expr '+' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Factor(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Expr(Box::new(Expr::Add { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::EXPR_2) => {
                // expr : expr '-' factor
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Factor(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Expr(Box::new(Expr::Sub { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::EXPR_3) => {
                // expr : factor
                match ast_stack.pop() {
                    Some(StackItem::Factor(factor)) => {
                        ast_stack.push(StackItem::Expr(Box::new(Expr::Factor(factor))));
                    }
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }

            ParseEvent::Reduce(RuleID::FACTOR_1) => {
                // factor : factor '*' term
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Term(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Factor(lhs)),
                    ) => ast_stack.push(StackItem::Factor(Box::new(Factor::Mul { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::FACTOR_2) => {
                // factor : factor '/' term
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Term(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Factor(lhs)),
                    ) => ast_stack.push(StackItem::Factor(Box::new(Factor::Div { lhs, op, rhs }))),
                    _ => anyhow::bail!("unexpected stack/parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::FACTOR_3) => {
                // factor : term
                match ast_stack.pop() {
                    Some(StackItem::Term(term)) => {
                        ast_stack.push(StackItem::Factor(Box::new(Factor::Term(term))));
                    }
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }

            ParseEvent::Reduce(RuleID::TERM_1) => {
                // term : num
                match args[0].take() {
                    Some(ParseItem::T(num)) => {
                        ast_stack.push(StackItem::Term(Box::new(Term::Num(num))));
                    }
                    _ => anyhow::bail!("unexpected parse item"),
                }
            }
            ParseEvent::Reduce(RuleID::TERM_2) => {
                // term : '(' expr ')'
                match (args[0].take(), ast_stack.pop(), args[2].take()) {
                    (
                        Some(ParseItem::T(l_paren)),
                        Some(StackItem::Expr(expr)),
                        Some(ParseItem::T(r_paren)),
                    ) => {
                        ast_stack.push(StackItem::Term(Box::new(Term::Paren {
                            l_paren,
                            expr,
                            r_paren,
                        })));
                    }
                    _ => anyhow::bail!("unexpected parse/stack item"),
                }
            }

            ParseEvent::Accept => {
                // $start: expr
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
