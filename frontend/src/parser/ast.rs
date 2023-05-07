
#[derive(Debug, PartialEq, Eq)]

pub enum BinOp{
    Plus,
    Minus,
    Divide,
    Multiply,
}
#[derive(PartialEq, Eq, Debug)]
pub enum Type{
    Int32,
    Char,
    Bool,
}


#[derive(Debug, PartialEq, Eq)]
pub enum Expr{
    BinExpr {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Term(Term),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Term{
    I32(i32),
    Var(String),    
}

impl From<i32> for Term{
    fn from(value: i32) -> Self {
        Term::I32(value)
    }
}
impl From<String> for Term{
    fn from(value: String) -> Self {
        Term::Var(value)
    }
}
impl From<&str> for Term{
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
            _ => panic!("invalid op")
        }
    }
}
impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Expr::Term(value.into())
    }
}



