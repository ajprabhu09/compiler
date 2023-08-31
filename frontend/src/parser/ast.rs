use std::{io, rc::Rc, str::FromStr, clone};

use super::grammer::ExprParser;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]

pub enum BinOp {
    Plus,
    Minus,
    Divide,
    Multiply,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Int32,
    Char,
    Bool,
    Unit,
    Func {
        params: Rc<Vec<Type>>,
        ret_type: Rc<Type>,
    },
    UnTyped,
}

type ExprNode = Rc<Expr>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    FuncExpr {
        ident: String,
        _type: Type,
        body: Rc<Expr>,
    },

    BinExpr {
        op: BinOp,
        lhs: Rc<Expr>,
        rhs: Rc<Expr>,
    },
    Term(Term),

    DeclVal {
        name: String,
        _type: Type,
    },

    DeclVar {
        name: String,
        _type: Type,
    },

    Assign {
        lval: Rc<Expr>,
        rval: Rc<Expr>,
        _type: Type,
    },

    BlockExpr {
        _type: Type,
        start: Option<Rc<BlockNode>>,
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockNode {
    pub curr: ExprNode,
    pub succ: Option<Rc<BlockNode>>
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    I32(i32),
    Var(String),    
}

impl From<i32> for Term {
    fn from(value: i32) -> Self {
        Term::I32(value)
    }
}
impl From<String> for Term {
    fn from(value: String) -> Self {
        Term::Var(value)
    }
}
impl From<&str> for Term {
    fn from(value: &str) -> Self {
        Term::Var(value.to_string())
    }
}

impl From<&str> for BinOp {
    fn from(value: &str) -> Self {
        match value {
            "+" => BinOp::Plus,
            "-" => BinOp::Minus,
            "/" => BinOp::Divide,
            "*" => BinOp::Multiply,
            _ => panic!("invalid op"),
        }
    }
}

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        let t: Term = value.into();
        Expr::Term(t)
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Expr::Term(value.into())
    }
}

impl FromStr for Expr {
    type Err = String;

    fn from_str(s: &str) -> Result<Expr, String> {
        ExprParser::new().parse(s).map_err(|e| format!("{}", e))
    }
}
