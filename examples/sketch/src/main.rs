use lelele_example_sketch::{
    self as me,
    syntax::{Ast, Expr, Term},
};
use lelele_runtime::parser::{ParseEvent, ParseItem};
use me::parser::RuleID;

enum StackItem<'t> {
    Ast(Ast<'t>),
    Expr(Expr<'t>),
    Term(Term<'t>),
}

fn main() -> anyhow::Result<()> {
    let mut tokens = me::lexer::tokens();
    let mut parser = me::parser::parser();
    let mut args = vec![];
    let mut ast_stack = vec![];
    loop {
        match parser.next_event(&mut tokens, &mut args)? {
            ParseEvent::Reduce(RuleID::R1) => {
                // A : E '=' E
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Expr(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Ast(Ast::Equal {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    })),
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }
            ParseEvent::Reduce(RuleID::R2) => {
                // A : ID
                let ident = match args[0].take() {
                    Some(ParseItem::T(t)) => t,
                    _ => anyhow::bail!("incorrect parse item"),
                };
                ast_stack.push(StackItem::Ast(Ast::Ident(ident)));
            }
            ParseEvent::Reduce(RuleID::R3) => {
                // E : E '+' T
                match (ast_stack.pop(), args[1].take(), ast_stack.pop()) {
                    (
                        Some(StackItem::Term(rhs)),
                        Some(ParseItem::T(op)),
                        Some(StackItem::Expr(lhs)),
                    ) => ast_stack.push(StackItem::Expr(Expr::Plus {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    })),
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }
            ParseEvent::Reduce(RuleID::R4) => {
                // E : T
                match ast_stack.pop() {
                    Some(StackItem::Term(t)) => {
                        ast_stack.push(StackItem::Expr(Expr::Term(Box::new(t))));
                    }
                    _ => anyhow::bail!("unexpected stack item"),
                }
            }
            ParseEvent::Reduce(RuleID::R5) => {
                // T : NUM
                let num = match args[0].take() {
                    Some(ParseItem::T(t)) => t,
                    _ => anyhow::bail!("incorrect parse item"),
                };
                ast_stack.push(StackItem::Term(Term::Num(num)));
            }
            ParseEvent::Reduce(RuleID::R6) => {
                // T : ID
                let ident = match args[0].take() {
                    Some(ParseItem::T(t)) => t,
                    _ => anyhow::bail!("incorrect parse item"),
                };
                ast_stack.push(StackItem::Term(Term::Ident(ident)));
            }
            ParseEvent::Accept => {
                // $start: A
                let ast = match ast_stack.pop() {
                    Some(StackItem::Ast(ast)) => ast,
                    _ => anyhow::bail!("unexpected stack item"),
                };
                println!("parsed: {:#?}", ast);
                break;
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}
