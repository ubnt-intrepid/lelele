#[derive(Debug)]
pub enum Expr<'source> {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Ident(&'source str),
    Tuple(Vec<Self>),
    App(Box<Self>, Vec<Self>),
    Unary(UnOp, Box<Self>),
    Binary(BinOp, Box<Self>, Box<Self>),
    If(Box<Self>, Box<Self>, Box<Self>),
    Let(Vec<&'source str>, Box<Self>, Box<Self>),
    LetRec(FunDef<'source>, Box<Self>),

    ArrayMake(Box<Self>, Box<Self>),
    ArrayGet(Box<Self>, Box<Self>),
    ArrayPut(Box<Self>, Box<Self>, Box<Self>),
}

#[derive(Debug, Copy, Clone)]
pub enum UnOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Sub,
    FAdd,
    FSub,
    FMul,
    FDiv,
    Equal,
    LessGreater,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug)]
pub struct FunDef<'source> {
    pub name: &'source str,
    pub args: Vec<&'source str>,
    pub body: Box<Expr<'source>>,
}
