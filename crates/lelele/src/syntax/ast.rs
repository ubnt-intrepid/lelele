use std::cmp::Ordering;

#[derive(Debug)]
pub struct File {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    TerminalDesc(TerminalDesc),
    NonterminalDesc(NonterminalDesc),
    RuleDesc(RuleDesc),
    PrecDesc(PrecDesc),
    StartDesc(StartDesc),
}
impl Stmt {
    pub fn cmp_by_desc(&self, other: &Self) -> Ordering {
        use Ordering::*;
        match self {
            Stmt::PrecDesc(..) => match other {
                Stmt::PrecDesc(..) => Equal,
                _ => Less,
            },
            Stmt::TerminalDesc(..) => match other {
                Stmt::PrecDesc(..) => Greater,
                Stmt::TerminalDesc(..) => Equal,
                _ => Less,
            },
            Stmt::NonterminalDesc(..) => match other {
                Stmt::RuleDesc(..) | Stmt::StartDesc(..) => Less,
                Stmt::NonterminalDesc(..) => Equal,
                Stmt::TerminalDesc(..) | Stmt::PrecDesc(..) => Greater,
            },
            Stmt::RuleDesc(..) => match other {
                Stmt::StartDesc(..) => Less,
                Stmt::RuleDesc(..) => Equal,
                _ => Greater,
            },
            Stmt::StartDesc(..) => match other {
                Stmt::StartDesc(..) => Equal,
                _ => Greater,
            },
        }
    }
}

#[derive(Debug)]
pub struct TerminalDesc {
    pub configs: Vec<Config>,
    pub idents: Vec<String>,
}

#[derive(Debug)]
pub struct NonterminalDesc {
    pub idents: Vec<String>,
}

#[derive(Debug)]
pub struct RuleDesc {
    pub left: String,
    pub productions: Vec<Production>,
}

#[derive(Debug)]
pub struct PrecDesc {
    pub configs: Vec<Config>,
    pub ident: String,
}

#[derive(Debug)]
pub struct StartDesc {
    pub name: String,
}

#[derive(Debug)]
pub struct Production {
    pub configs: Vec<Config>,
    pub elems: Vec<ProductionElem>,
}

#[derive(Debug)]
pub enum ProductionElem {
    Ident(String),
    ErrorToken,
}

#[derive(Debug)]
pub struct Config {
    pub key: String,
    pub value: String,
}
