#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StringConst(String),
    IntConst(i32),
    Nil,
    LValue(LValue),
    Minus(Box<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),

    Assign {
        name: LValue,
        exp: Box<Expr>,
    },

    CallExp {
        func: String,
        exprs: Vec<Expr>,
    },

    SeqExp(Vec<Expr>),

    RecordExp {
        typ: String,
        fields: Vec<(String, Expr)>, // (field name, expr)
    },

    ArrayExp {
        typ: String,
        size: Box<Expr>,
        init: Box<Expr>,
    },

    IfThen {
        test: Box<Expr>,
        then_: Box<Expr>,
    },

    IfThenElse {
        test: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },

    WhileExp {
        test: Box<Expr>,
        body: Box<Expr>,
    },

    ForExp {
        var: String,
        lo: Box<Expr>,
        hi: Box<Expr>,
        body: Box<Expr>,
    },

    Break,

    LetExp {
        decs: Vec<Dec>,
        body: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Simple(String),
    Field(Box<LValue>, String),
    Subscript(Box<LValue>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Dec {
    Function(Vec<FunDec>),
    Var { name: String, typ: Option<String>, init: Box<Expr> },
    Type(Vec<(String, Ty)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDec {
    pub name: String,
    pub params: Vec<(String, String)>, // (param name, type name)
    pub result: Option<String>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Name(String),
    Record(Vec<(String, String)>), // (field name, type name)
    Array(String),
}
