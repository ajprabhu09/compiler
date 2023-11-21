use std::{boxed, fmt::Debug, io::Empty, ops::Range, rc::Rc, vec};

use logos::Lexer;

use crate::{
    srcfile::SrcFile,
    token::{self, Token},
};
use Token::*;

#[derive(Debug)]
pub struct Seq<'a> {
    // span: Range<usize>,
    pub data: Rc<Expr<'a>>,
    pub next: Option<Rc<Seq<'a>>>,
}

impl<'a> Seq<'a> {
    pub fn empty() -> Self {
        Self {
            data: Expr::Empty.into(),
            next: None,
        }
    }

    pub fn push_expr(mut self, e: Expr<'a>) -> Self {
        if self.data.empty() {
            self.data = e.into();
            self.next = None;
            self
        } else {
            self.next = Some(Seq::empty().push_expr(e).into());
            self
        }
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
    },
    FuncDef {
        name: SrcInfo<'source>,
        args: Vec<Param<'source>>,
        block: Seq<'source>,
    },
    Block(Seq<'source>),
    Scope {},
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

#[derive(Debug)]
pub struct ParsingErr; // TODO extend with good errors

pub struct Ast {
    src: SrcFile,
}

type PResult<T> = Result<T, ParsingErr>;

fn unwrap_next_token(x: Option<Result<Token, ()>>) -> PResult<Token> {
    if let Some(Ok(token)) = x {
        return Ok(token);
    } else {
        return Err(ParsingErr);
    }
}

fn peek<'a>(lexr: &mut Lexer<'a, Token>) -> Option<PResult<Token>> {
    let res = (lexr.clone()).next(); // Is this zero cost ?
    res.map(|x| x.map_err(|_| ParsingErr))
}

fn peek_assert<'a>(lexr: &mut Lexer<'a, Token>, assertee: Token) -> Option<bool> {
    let res = peek(lexr);
    // println!("peeking {:?} {:?}", res, lexr.slice());
    return res.map(|x| x.map_or_else(|e| false, |x| x == assertee));
}

fn next_and_assert<'a>(lexr: &mut Lexer<'a, Token>, assertee: Token) -> PResult<SrcInfo<'a>> {
    let token = unwrap_next_token(lexr.next())?;
    // println!("parsing - {:?} {:?}", token, lexr.slice());
    if token != assertee {
        return Err(ParsingErr);
    }
    return Ok(lexr.into());
}

impl Ast {
    fn parse_param_list<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Vec<Param<'a>>> {
        let mut params = vec![];
        loop {
            let param_name = next_and_assert(lexr, Token::Ident)?;
            next_and_assert(lexr, Token::Colon)?;
            let typ = next_and_assert(lexr, Token::Ident)?;
            params.push(Param {
                arg_name: param_name,
                typ: Type(typ),
            });
            if peek_assert(lexr, CloseParen) == Some(true) {
                break;
            } else if peek_assert(lexr, Comma) == Some(true) {
                next_and_assert(lexr, Comma)?;
                continue;
            } else {
                return Err(ParsingErr);
            }
        } // take and ignore
        Ok(params)
    }




    fn parse_stmt<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let stmt = Expr::Empty;
        loop {
            if peek_assert(lexr, Semicolon) == Some(true) {
                next_and_assert(lexr, Semicolon)?;
                return Ok(stmt)
            }
        }
    }

    fn parse_var_decl<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let var_name = next_and_assert(lexr, Ident)?;
        next_and_assert(lexr, Colon)?;
        let _type = next_and_assert(lexr, Ident)?;

        let decl = if peek_assert(lexr, Eq) == Some(true) {
            next_and_assert(lexr, Eq)?;
            Expr::Decl {
                typ: Type(_type),
                name: var_name,
                value: Some(Self::parse_to_expr(lexr)?.into()),
            }
        } else {
            next_and_assert(lexr, Semicolon)?;
            Expr::Decl {
                typ: Type(_type),
                name: var_name,
                value: None,
            }
        };
        Ok(decl)
    }

    fn parse_fun_body<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Seq<'a>> {
        next_and_assert(lexr, OpenBrace)?;
        // TODO: parse body seq
        let mut block = Seq::empty();
        loop {
            if peek_assert(lexr, CloseBrace) == Some(true) {
                next_and_assert(lexr, CloseBrace)?;
                return Ok(block);
            }
            let expr = Self::parse_to_expr(lexr)?;
            block = block.push_expr(expr);
        }
    }

    fn parse_func<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let name = next_and_assert(lexr, Token::Ident)?;
        next_and_assert(lexr, Token::OpenParen)?;
        let mut params = vec![];
        loop {
            if peek_assert(lexr, CloseParen) == Some(true) {
                next_and_assert(lexr, CloseParen)?;
                let body = Self::parse_fun_body(lexr)?;
                return Ok(Expr::FuncDef {
                    name: name,
                    args: params,
                    block: body,
                });
            }
            params = Self::parse_param_list(lexr)?;
        }
        return Err(ParsingErr);
    }

    // fn parse<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
    //     let mut tokens = vec![];
    //     loop {
    //         if peek_assert(lexr, Semicolon) == Some(true) {
    //             println!("{:?}", tokens);
    //             return Ok(Expr::Empty);
    //         }
    //         tokens.push(Self::parse_to_expr(lexr));
    //     }
    //     return Err(ParsingErr);
    // }

    fn parse_to_expr<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let token = lexr.next();
        if let Some(token) = token &&
            let Ok (token) = token {
            let expr = match token {
                Integer =>  Expr::Lit(lexr.into()),
                Func => Self::parse_func(lexr)?,
                Struct => todo!(),
                OpenBrace => todo!(),
                CloseBrace => todo!(),
                OpenParen => todo!(),
                CloseParen => todo!(),
                Eq => todo!(),
                Plus => todo!(),
                Minus => todo!(),
                Star => todo!(),
                ForwardSlash => todo!(),
                Ident => Expr::Ident(lexr.into()),
                Colon => todo!(),
                Comma => todo!(),
                Semicolon => Expr::Empty,
                Return => Expr::Return { expr: Self::parse_to_expr(lexr)?.into() },
                Var => Self::parse_var_decl(lexr)?,
            };
            return Ok(expr)
        }
        Err(ParsingErr)
    }

    pub fn from(src: SrcFile) -> Self {
        Self { src }
    }
    pub fn ast(&self) -> PResult<Expr<'_>> {
        Self::parse_to_expr(&mut self.src.lexer())
    }
}
