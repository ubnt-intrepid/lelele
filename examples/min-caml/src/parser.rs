mod gen {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}
pub use gen::*;

use crate::{
    lexer::{Lexer, Token},
    syntax::{BinOp, Expr, FunDef, UnOp},
};
use anyhow::{ensure, Context as _};
use lelele_runtime::engine::{ParseEngine, ParseEvent, Symbol::*};

#[derive(Debug)]
enum StackItem<'source> {
    Expr(Expr<'source>),
    FunDef(FunDef<'source>),
    FormalArgs(Vec<&'source str>),
    ActualArgs(Vec<Expr<'source>>),
    Elems(Vec<Expr<'source>>),
    Pat(Vec<&'source str>),
}
use StackItem::*;

pub fn parse(input: &str) -> anyhow::Result<Expr<'_>> {
    let span = tracing::trace_span!("parse");
    let _entered = span.enter();

    let mut lexer = Lexer::new(input);
    let mut engine = ParseEngine::new(ParserDef::default());
    let mut stack: Vec<StackItem> = vec![];
    macro_rules! pop_stack {
        ($Variant:ident) => {
            match stack.pop() {
                Some(StackItem::$Variant(v)) => v,
                _ => anyhow::bail!("unexpected stack item (expected: {})", stringify!($Variant)),
            }
        };
    }

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
                tracing::trace!("shifting: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(symbol, args) => {
                tracing::trace!("reduce: {:?} -> {:?}", symbol, args);
                match (symbol, args) {
                    (Symbol::SimpleExpr, [T(Token::LParen), N(Symbol::Expr), T(Token::RParen)]) => {
                        ensure!(
                            matches!(stack.last(), Some(Expr(..))),
                            "unexpected stack item (expected: Expr)"
                        );
                    }
                    (Symbol::SimpleExpr, [T(Token::LParen), T(Token::RParen)]) => {
                        stack.push(Expr(Expr::Unit));
                    }
                    (Symbol::SimpleExpr, [T(Token::True)]) => {
                        stack.push(Expr(Expr::Bool(true)));
                    }
                    (Symbol::SimpleExpr, [T(Token::False)]) => {
                        stack.push(Expr(Expr::Bool(true)));
                    }
                    (Symbol::SimpleExpr, [T(Token::Int(lit))]) => {
                        let lit: i64 = lit.parse().context("parsing integer literal")?;
                        stack.push(Expr(Expr::Int(lit)));
                    }
                    (Symbol::SimpleExpr, [T(Token::Float(lit))]) => {
                        let lit: f64 = lit.parse().context("parsing integer literal")?;
                        stack.push(Expr(Expr::Float(lit)));
                    }
                    (Symbol::SimpleExpr, [T(Token::Ident(var))]) => {
                        stack.push(Expr(Expr::Ident(var)));
                    }
                    (
                        Symbol::SimpleExpr,
                        [N(Symbol::SimpleExpr), T(Token::Dot), T(Token::LParen), N(Symbol::Expr), T(Token::RParen)],
                    ) => {
                        let index = pop_stack!(Expr);
                        let array = pop_stack!(Expr);
                        stack.push(Expr(Expr::ArrayGet(Box::new(array), Box::new(index))));
                    }

                    (Symbol::Expr, [N(Symbol::SimpleExpr)]) => {
                        ensure!(
                            matches!(stack.last(), Some(Expr(..))),
                            "unexpected stack item (expected: Expr)"
                        );
                    }
                    (Symbol::Expr, [T(t_op), N(Symbol::Expr)]) => {
                        let expr = pop_stack!(Expr);
                        let op = match t_op {
                            Token::Not => UnOp::Not,
                            Token::Minus => UnOp::Neg,
                            Token::MinusDot => UnOp::FNeg,
                            _ => unreachable!(),
                        };
                        stack.push(Expr(Expr::Unary(op, Box::new(expr))));
                    }
                    (Symbol::Expr, [N(Symbol::Expr), T(t_op), N(Symbol::Expr)])
                        if !matches!(t_op, Token::Semicolon) =>
                    {
                        let rhs = pop_stack!(Expr);
                        let lhs = pop_stack!(Expr);
                        let op = match t_op {
                            Token::Plus => BinOp::Add,
                            Token::Minus => BinOp::Sub,
                            Token::PlusDot => BinOp::FAdd,
                            Token::MinusDot => BinOp::FSub,
                            Token::StarDot => BinOp::FMul,
                            Token::SlashDot => BinOp::FDiv,
                            Token::Equal => BinOp::Equal,
                            Token::LessGreater => BinOp::Greater,
                            Token::Less => BinOp::Less,
                            Token::Greater => BinOp::Greater,
                            Token::LessEqual => BinOp::LessEqual,
                            Token::GreaterEqual => BinOp::GreaterEqual,
                            _ => unreachable!(),
                        };
                        stack.push(Expr(Expr::Binary(op, Box::new(lhs), Box::new(rhs))));
                    }
                    (
                        Symbol::Expr,
                        [T(Token::If), N(Symbol::Expr), T(Token::Then), N(Symbol::Expr), T(Token::Else), N(Symbol::Expr)],
                    ) => {
                        let e3 = pop_stack!(Expr);
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Expr(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))));
                    }
                    (
                        Symbol::Expr,
                        [T(Token::Let), T(Token::Ident(name)), T(Token::Equal), N(Symbol::Expr), T(Token::In), N(Symbol::Expr)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let value = pop_stack!(Expr);
                        stack.push(Expr(Expr::Let(vec![name], Box::new(value), Box::new(body))));
                    }
                    (
                        Symbol::Expr,
                        [T(Token::Let), T(Token::LParen), N(Symbol::Pat), T(Token::RParen), T(Token::Equal), N(Symbol::Expr), T(Token::In), N(Symbol::Expr)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let value = pop_stack!(Expr);
                        let pat = pop_stack!(Pat);
                        stack.push(Expr(Expr::Let(pat, Box::new(value), Box::new(body))));
                    }
                    (
                        Symbol::Expr,
                        [T(Token::Let), T(Token::Rec), N(Symbol::FunDef), T(Token::In), N(Symbol::Expr)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let fundef = pop_stack!(FunDef);
                        stack.push(Expr(Expr::LetRec(fundef, Box::new(body))));
                    }
                    (Symbol::Expr, [N(Symbol::SimpleExpr), N(Symbol::ActualArgs)]) => {
                        let args = pop_stack!(ActualArgs);
                        let caller = pop_stack!(Expr);
                        stack.push(Expr(Expr::App(Box::new(caller), args)));
                    }
                    (Symbol::Expr, [N(Symbol::Elems)]) => {
                        let mut elems = pop_stack!(Elems);
                        elems.reverse();
                        stack.push(Expr(Expr::Tuple(elems)));
                    }
                    (
                        Symbol::Expr,
                        [N(Symbol::SimpleExpr), T(Token::Dot), T(Token::LParen), N(Symbol::Expr), T(Token::RParen), T(Token::LessMinus), N(Symbol::Expr)],
                    ) => {
                        let value = pop_stack!(Expr);
                        let index = pop_stack!(Expr);
                        let array = pop_stack!(Expr);
                        stack.push(Expr(Expr::ArrayPut(
                            Box::new(array),
                            Box::new(index),
                            Box::new(value),
                        )));
                    }
                    (Symbol::Expr, [N(Symbol::Expr), T(Token::Semicolon), N(Symbol::Expr)]) => {
                        // e1; e2  --> let () = e1 in e2
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Expr(Expr::Let(vec![], Box::new(e1), Box::new(e2))));
                    }
                    (
                        Symbol::Expr,
                        [T(Token::ArrayMake), N(Symbol::SimpleExpr), N(Symbol::SimpleExpr)],
                    ) => {
                        let default = pop_stack!(Expr);
                        let length = pop_stack!(Expr);
                        stack.push(Expr(Expr::ArrayMake(Box::new(length), Box::new(default))));
                    }

                    (
                        Symbol::FunDef,
                        [T(Token::Ident(name)), N(Symbol::FormalArgs), T(Token::Equal), N(Symbol::Expr)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let mut args = pop_stack!(FormalArgs);
                        args.reverse();
                        stack.push(FunDef(FunDef {
                            name,
                            args,
                            body: Box::new(body),
                        }));
                    }

                    (Symbol::FormalArgs, [T(Token::Ident(arg)), N(Symbol::FormalArgs)]) => {
                        match stack.last_mut() {
                            Some(FormalArgs(args)) => {
                                args.push(arg);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: FormalArgs)"),
                        }
                    }
                    (Symbol::FormalArgs, [T(Token::Ident(arg))]) => {
                        stack.push(StackItem::FormalArgs(vec![arg]));
                    }

                    (Symbol::ActualArgs, [N(Symbol::ActualArgs), N(Symbol::SimpleExpr)]) => {
                        let arg = pop_stack!(Expr);
                        match stack.last_mut() {
                            Some(ActualArgs(args)) => {
                                args.push(arg);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: ActualArgs)"),
                        }
                    }
                    (Symbol::ActualArgs, [N(Symbol::SimpleExpr)]) => {
                        let arg = pop_stack!(Expr);
                        stack.push(ActualArgs(vec![arg]));
                    }

                    (Symbol::Elems, [N(Symbol::Elems), T(Token::Comma), N(Symbol::Expr)]) => {
                        let elem = pop_stack!(Expr);
                        match stack.last_mut() {
                            Some(Elems(elems)) => {
                                elems.push(elem);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: Elems)"),
                        }
                    }
                    (Symbol::Elems, [N(Symbol::Expr), T(Token::Comma), N(Symbol::Expr)]) => {
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Elems(vec![e1, e2]));
                    }

                    (Symbol::Pat, [N(Symbol::Pat), T(Token::Comma), T(Token::Ident(p))]) => {
                        match stack.last_mut() {
                            Some(Pat(patterns)) => {
                                patterns.push(p);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: Pat)"),
                        }
                    }
                    (Symbol::Pat, [T(Token::Ident(p1)), T(Token::Comma), T(Token::Ident(p2))]) => {
                        stack.push(Pat(vec![p1, p2]));
                    }

                    _ => unreachable!(),
                }
            }

            ParseEvent::HandlingError {
                state,
                lookahead,
                expected,
            } => {
                tracing::trace!(
                    "handling error: lr_state={:?}, lookahead={:?}, expected={:?}",
                    state,
                    lookahead,
                    expected,
                );

                continue;
            }

            ParseEvent::Accepted(..) => {
                tracing::trace!("accepted");
                break Ok(pop_stack!(Expr));
            }

            ParseEvent::Rejected => {
                tracing::trace!("rejected");
                anyhow::bail!("rejected");
            }
        }
    }
}
