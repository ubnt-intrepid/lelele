#[derive(Debug)]
pub struct Grammar {
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
