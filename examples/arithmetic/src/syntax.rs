//! Syntax definition.

use crate::lexer::Token;
use std::fmt;

#[derive(Debug)]
pub enum Expr<'source> {
    Add {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Expr<'source>>,
    },
    Sub {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Expr<'source>>,
    },
    Mul {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Expr<'source>>,
    },
    Div {
        lhs: Box<Expr<'source>>,
        op: Token<'source>,
        rhs: Box<Expr<'source>>,
    },
    Num(&'source str),
    Paren {
        l_paren: Token<'source>,
        expr: Box<Expr<'source>>,
        r_paren: Token<'source>,
    },
    Neg {
        minus: Token<'source>,
        expr: Box<Expr<'source>>,
    },
    Error,
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add { lhs, rhs, .. } => write!(f, "(+ {} {})", lhs, rhs),
            Self::Sub { lhs, rhs, .. } => write!(f, "(- {} {})", lhs, rhs),
            Self::Mul { lhs, rhs, .. } => write!(f, "(* {} {})", lhs, rhs),
            Self::Div { lhs, rhs, .. } => write!(f, "(/ {} {})", lhs, rhs),
            Self::Num(n) => write!(f, "{}", n),
            Self::Paren { expr, .. } => write!(f, "({})", expr),
            Self::Neg { expr, .. } => write!(f, "(neg {})", expr),
            Self::Error => write!(f, "<error>"),
        }
    }
}
