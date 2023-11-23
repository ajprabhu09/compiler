use core::fmt;
use std::{boxed, fmt::Debug, io::Empty, ops::Range, rc::Rc, vec, assert_matches::assert_matches, collections::LinkedList};

use logos::Lexer;

use crate::{
    srcfile::SrcFile,
    token::{self, Token}, parsing::{PResult, ParsingErr, Match, Parseable, Consume, self, ZeroOrMore},
};
use Token::*;

pub enum List<'a> {
    Cons(Rc<Expr<'a>>, Rc<Seq<'a>>),
    Nil
}

#[derive(Debug)]
pub struct Seq<'a> {
   pub container: LinkedList<Rc<Expr<'a>>>
}

impl<'a> Seq<'a> {
    pub fn empty() -> Self {
        Self { container: LinkedList::new() }
    }
    pub fn push_expr(mut self, e: Expr<'a>) -> Self {
        self.container.push_back(e.into());
        self
    }
}




#[derive(Debug)]
pub struct Type<'source>(SrcInfo<'source>);

#[derive(Debug)]
pub struct Param<'source> {
    pub arg_name: SrcInfo<'source>,
    pub typ: Type<'source>,
}

pub struct SrcInfo<'a> {
    span: Range<usize>,
    data: &'a str,
}

impl<'a> PartialEq for SrcInfo<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}


impl<'a> SrcInfo<'a> {
    pub fn from(data: &'a str) -> Self {
        Self { span: 0..data.len(), data: data }
    }

    pub fn eqv(&self, other: &'a str) -> bool {
        self.data == other
    }
}

impl<'a> Debug for SrcInfo<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.debug_struct("SrcInfo").field("span", &self.span).field("data", &self.data).finish()
        write!(f, "{}", self.data)
    }
}

impl<'a> Into<SrcInfo<'a>> for Lexer<'a, Token> {
    fn into(self) -> SrcInfo<'a> {
        SrcInfo {
            span: self.span(),
            data: self.slice(),
        }
    }
}

impl<'a> From<&mut Lexer<'a, Token>> for SrcInfo<'a> {
    fn from(value: &mut Lexer<'a, Token>) -> Self {
        Self {
            span: value.span(),
            data: value.slice(),
        }
    }
}
impl<'a> From<&'a str> for SrcInfo<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            data: value,
            span: 0..value.len(),
        }
    }
}


#[derive(Debug)]
pub enum Fixity {
    Infix,
    Prefix,
}



#[derive(Debug)]
pub enum Expr<'source> {



    Type(Type<'source>), // This ideally be a reference to a previously defined type

    Ident(SrcInfo<'source>),
    Lit(SrcInfo<'source>),
    Decl {
        typ: Type<'source>,
        name: SrcInfo<'source>,
        value: Option<Rc<Expr<'source>>>,
    },

    FuncCall {
        op: SrcInfo<'source>,
        args: Vec<Rc<Expr<'source>>>,
        fixity: Fixity,
        pure: bool
    },
    FuncDef {
        name: SrcInfo<'source>,
        args: Vec<Param<'source>>,
        block: Seq<'source>,
    },
    Block(Seq<'source>),
    TopLevel(Seq<'source>),
    Empty,
    Return {
        expr: Rc<Expr<'source>>,
    },
}

impl<'a> Expr<'a> {
    pub fn empty(&self) -> bool {
        match self {
            Expr::Empty => true,
            _ => false,
        }
    }
}


pub struct Ast {
    src: SrcFile,
}



impl Ast {


    fn function_parser<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let func_keyword = Consume(Token::Func);
        let func_name = Match(Token::Ident, |lexr| {
            let function_name: SrcInfo<'_> = lexr.into();
            Ok(Expr::FuncDef { name: function_name, args: vec![], block: Seq::empty() })
        });

        let binding = parsing::Seq(
                &func_name,
                &parsing::Seq(
                    &Consume(Token::OpenParen),
                    &Consume(Token::CloseParen),
                    |a,b| {Ok(Expr::Empty)}
                ),
                |a,b| {
                    assert_matches!(a, Expr::FuncDef { .. });
                    Ok(a)
                },
            );
        let function = parsing::Seq(
            &func_keyword,
            &binding,
            |a,b| { assert_matches!(b, Expr::FuncDef { .. }); Ok(b) }
        );
        let zero_ormore = ZeroOrMore(&function, |a, b| {
            if let Expr::Empty = a {
                return Ok(Expr::TopLevel(Seq::empty().push_expr(b)));
            }
            if let Expr::TopLevel(seq) = a {
                return Ok(Expr::TopLevel(seq.push_expr(b)));
            }
            return Err(ParsingErr);
        });

        zero_ormore.consume_parse(lexr).unwrap()

    }

    fn parse<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        Self::function_parser(lexr)
    }
    pub fn from(src: SrcFile) -> Self {
        Self { src }
    }
    pub fn ast(&self) -> PResult<Expr<'_>> {
        Self::parse(&mut self.src.lexer())
    }

}
