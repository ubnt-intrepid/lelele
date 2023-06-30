mod gen {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}
pub use gen::*;

use crate::{
    lexer::{Lexer, Token},
    syntax::{BinOp, Expr, FunDef, UnOp},
};
use anyhow::{ensure, Context as _};
use lelele_runtime::parser::{ParseEvent, ParseItem::*, Parser};

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
    let mut parser = Parser::new(ParserDef::default());
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
        let event = parser.resume()?;
        match event {
            ParseEvent::InputNeeded => match lexer.next() {
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
                tracing::trace!("shifting: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(symbol, args) => {
                tracing::trace!("reduce: {:?} -> {:?}", symbol, args);
                match (symbol, args) {
                    (
                        SymbolID::SIMPLE_EXP,
                        [T(Token::LParen), N(SymbolID::EXP), T(Token::RParen)],
                    ) => {
                        ensure!(
                            matches!(stack.last(), Some(Expr(..))),
                            "unexpected stack item (expected: Expr)"
                        );
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::LParen), T(Token::RParen)]) => {
                        stack.push(Expr(Expr::Unit));
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::True)]) => {
                        stack.push(Expr(Expr::Bool(true)));
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::False)]) => {
                        stack.push(Expr(Expr::Bool(true)));
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::Int(lit))]) => {
                        let lit: i64 = lit.parse().context("parsing integer literal")?;
                        stack.push(Expr(Expr::Int(lit)));
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::Float(lit))]) => {
                        let lit: f64 = lit.parse().context("parsing integer literal")?;
                        stack.push(Expr(Expr::Float(lit)));
                    }
                    (SymbolID::SIMPLE_EXP, [T(Token::Ident(var))]) => {
                        stack.push(Expr(Expr::Ident(var)));
                    }
                    (
                        SymbolID::SIMPLE_EXP,
                        [N(SymbolID::SIMPLE_EXP), T(Token::Dot), T(Token::LParen), N(SymbolID::EXP), T(Token::RParen)],
                    ) => {
                        let index = pop_stack!(Expr);
                        let array = pop_stack!(Expr);
                        stack.push(Expr(Expr::ArrayGet(Box::new(array), Box::new(index))));
                    }

                    (SymbolID::EXP, [N(SymbolID::SIMPLE_EXP)]) => {
                        ensure!(
                            matches!(stack.last(), Some(Expr(..))),
                            "unexpected stack item (expected: Expr)"
                        );
                    }
                    (SymbolID::EXP, [T(t_op), N(SymbolID::EXP)]) => {
                        let expr = pop_stack!(Expr);
                        let op = match t_op {
                            Token::Not => UnOp::Not,
                            Token::Minus => UnOp::Neg,
                            Token::MinusDot => UnOp::FNeg,
                            _ => unreachable!(),
                        };
                        stack.push(Expr(Expr::Unary(op, Box::new(expr))));
                    }
                    (SymbolID::EXP, [N(SymbolID::EXP), T(t_op), N(SymbolID::EXP)])
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
                        SymbolID::EXP,
                        [T(Token::If), N(SymbolID::EXP), T(Token::Then), N(SymbolID::EXP), T(Token::Else), N(SymbolID::EXP)],
                    ) => {
                        let e3 = pop_stack!(Expr);
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Expr(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3))));
                    }
                    (
                        SymbolID::EXP,
                        [T(Token::Let), T(Token::Ident(name)), T(Token::Equal), N(SymbolID::EXP), T(Token::In), N(SymbolID::EXP)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let value = pop_stack!(Expr);
                        stack.push(Expr(Expr::Let(vec![name], Box::new(value), Box::new(body))));
                    }
                    (
                        SymbolID::EXP,
                        [T(Token::Let), T(Token::LParen), N(SymbolID::PAT), T(Token::RParen), T(Token::Equal), N(SymbolID::EXP), T(Token::In), N(SymbolID::EXP)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let value = pop_stack!(Expr);
                        let pat = pop_stack!(Pat);
                        stack.push(Expr(Expr::Let(pat, Box::new(value), Box::new(body))));
                    }
                    (
                        SymbolID::EXP,
                        [T(Token::Let), T(Token::Rec), N(SymbolID::FUNDEF), T(Token::In), N(SymbolID::EXP)],
                    ) => {
                        let body = pop_stack!(Expr);
                        let fundef = pop_stack!(FunDef);
                        stack.push(Expr(Expr::LetRec(fundef, Box::new(body))));
                    }
                    (SymbolID::EXP, [N(SymbolID::SIMPLE_EXP), N(SymbolID::ACTUAL_ARGS)]) => {
                        let args = pop_stack!(ActualArgs);
                        let caller = pop_stack!(Expr);
                        stack.push(Expr(Expr::App(Box::new(caller), args)));
                    }
                    (SymbolID::EXP, [N(SymbolID::ELEMS)]) => {
                        let mut elems = pop_stack!(Elems);
                        elems.reverse();
                        stack.push(Expr(Expr::Tuple(elems)));
                    }
                    (
                        SymbolID::EXP,
                        [N(SymbolID::SIMPLE_EXP), T(Token::Dot), T(Token::LParen), N(SymbolID::EXP), T(Token::RParen), T(Token::LessMinus), N(SymbolID::EXP)],
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
                    (SymbolID::EXP, [N(SymbolID::EXP), T(Token::Semicolon), N(SymbolID::EXP)]) => {
                        // e1; e2  --> let () = e1 in e2
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Expr(Expr::Let(vec![], Box::new(e1), Box::new(e2))));
                    }
                    (
                        SymbolID::EXP,
                        [T(Token::ArrayMake), N(SymbolID::SIMPLE_EXP), N(SymbolID::SIMPLE_EXP)],
                    ) => {
                        let default = pop_stack!(Expr);
                        let length = pop_stack!(Expr);
                        stack.push(Expr(Expr::ArrayMake(Box::new(length), Box::new(default))));
                    }

                    (
                        SymbolID::FUNDEF,
                        [T(Token::Ident(name)), N(SymbolID::FORMAL_ARGS), T(Token::Equal), N(SymbolID::EXP)],
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

                    (SymbolID::FORMAL_ARGS, [T(Token::Ident(arg)), N(SymbolID::FORMAL_ARGS)]) => {
                        match stack.last_mut() {
                            Some(FormalArgs(args)) => {
                                args.push(arg);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: FormalArgs)"),
                        }
                    }
                    (SymbolID::FORMAL_ARGS, [T(Token::Ident(arg))]) => {
                        stack.push(StackItem::FormalArgs(vec![arg]));
                    }

                    (
                        SymbolID::ACTUAL_ARGS,
                        [N(SymbolID::ACTUAL_ARGS), N(SymbolID::SIMPLE_EXP)],
                    ) => {
                        let arg = pop_stack!(Expr);
                        match stack.last_mut() {
                            Some(ActualArgs(args)) => {
                                args.push(arg);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: ActualArgs)"),
                        }
                    }
                    (SymbolID::ACTUAL_ARGS, [N(SymbolID::SIMPLE_EXP)]) => {
                        let arg = pop_stack!(Expr);
                        stack.push(ActualArgs(vec![arg]));
                    }

                    (SymbolID::ELEMS, [N(SymbolID::ELEMS), T(Token::Comma), N(SymbolID::EXP)]) => {
                        let elem = pop_stack!(Expr);
                        match stack.last_mut() {
                            Some(Elems(elems)) => {
                                elems.push(elem);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: Elems)"),
                        }
                    }
                    (SymbolID::ELEMS, [N(SymbolID::EXP), T(Token::Comma), N(SymbolID::EXP)]) => {
                        let e2 = pop_stack!(Expr);
                        let e1 = pop_stack!(Expr);
                        stack.push(Elems(vec![e1, e2]));
                    }

                    (SymbolID::PAT, [N(SymbolID::PAT), T(Token::Comma), T(Token::Ident(p))]) => {
                        match stack.last_mut() {
                            Some(Pat(patterns)) => {
                                patterns.push(p);
                            }
                            _ => anyhow::bail!("unexpected stack item (expected: Pat)"),
                        }
                    }
                    (
                        SymbolID::PAT,
                        [T(Token::Ident(p1)), T(Token::Comma), T(Token::Ident(p2))],
                    ) => {
                        stack.push(Pat(vec![p1, p2]));
                    }

                    _ => unreachable!(),
                }
            }

            ParseEvent::AboutToAccept(arg) => {
                tracing::trace!("accept: {:?}", arg);
                ensure!(
                    matches!(stack.last(), Some(Expr(..))),
                    "unexpected stack item (expected: Expr)"
                );
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
                break Ok(pop_stack!(Expr));
            }

            ParseEvent::Rejected => {
                tracing::trace!("rejected");
                anyhow::bail!("rejected");
            }
        }
    }
}
