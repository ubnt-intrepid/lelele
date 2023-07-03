#[derive(Debug)]
pub struct Grammar {
    pub descs: Vec<Desc>,
}

#[derive(Debug)]
pub enum Desc {
    Terminal(TerminalDesc),
    Nonterminal(NonterminalDesc),
    Rule(RuleDesc),
    Prec(PrecDesc),
    Start(StartDesc),
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
    pub elems: Vec<String>,
}

#[derive(Debug)]
pub struct Config {
    pub key: String,
    pub value: String,
}
