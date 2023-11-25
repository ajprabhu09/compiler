use core::fmt;
use std::{boxed, fmt::Debug, io::Empty, ops::Range, rc::Rc, vec, assert_matches::assert_matches, collections::LinkedList, process::id};

use logos::Lexer;

use crate::{
    srcfile::SrcFile,
    token::{self, Token}, parsing::{PResult, ParsingErr, Match, Parseable, Consume, self, ZeroOrMore, MatchSeq, empty_expr, empty_combine, second, Optional, identity, first, collect, Either},
};
use Token::*;
use parsing::DebugParseable;
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
    pub fn push_back_expr(mut self, e: Expr<'a>) -> Self {
        self.container.push_back(e.into());
        self
    }
    pub fn push_front_expr(mut self, e: Expr<'a>) -> Self {
        self.container.push_front(e.into());
        self
    }

}




#[derive(Debug, Clone)]
pub struct Type<'source>(SrcInfo<'source>);

#[derive(Debug)]
pub struct Param<'source> {
    pub arg_name: SrcInfo<'source>,
    pub typ: Type<'source>,
}


#[derive(Clone)]
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
        value: Rc<Expr<'source>>,
    },
    FuncCall {
        op: SrcInfo<'source>,
        args: Vec<Rc<Expr<'source>>>,
        fixity: Fixity,
        pure: bool
    },
    FuncDef {
        name: SrcInfo<'source>,
        args: LinkedList<Param<'source>>,
        block: Seq<'source>,
    },
    // Block(Seq<'source>),
    // TopLevel(Seq<'source>),
    List(Seq<'source>),
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

    fn integer_parser<'a>() -> impl DebugParseable<'a> {
        let int_liter = Match(Token::Integer, |lexr| {
            Ok(Expr::Lit(lexr.into()))
        });
        int_liter
    }

    fn ident_parser<'a>() -> impl DebugParseable<'a> {
        let ident_parser = Match(Token::Ident, |lexr| Ok(Expr::Ident(lexr.into())));
        return ident_parser
    }

    fn parse_type_annotation<'a>() -> impl DebugParseable<'a> {
        let binding = MatchSeq(
            Self::ident_parser(),
            MatchSeq(
                Consume(Token::Colon),
                Self::ident_parser(),
                second()
            ),
            |a, b| { 
                if let Expr::Ident(name) = a {
                    if let Expr::Ident(type_) = b {
                        return Ok(Expr::Decl { typ: Type(type_), name: name, value: Expr::Empty.into() })
                    }
                }
                return Err(ParsingErr("unable to parse type annotation".into()))
            }
        );
        return binding;
    }
    fn param_list<'a>() -> impl DebugParseable<'a> {
        let binding = MatchSeq(
            Consume(Token::OpenParen),
            MatchSeq(
                MatchSeq(
                    Optional(Self::parse_type_annotation(), identity()),
                    ZeroOrMore(
                        MatchSeq(
                            Consume(Token::Comma),
                            Self::parse_type_annotation(),
                            second(),
                        ),
                        collect(),
                    ),
                    |a,b| {
                        if let Expr::Empty = a {
                            return Ok(Expr::Empty);
                        }
                        if let Expr::List(seq) = b {
                            return Ok(Expr::List(seq.push_front_expr(a)));
                        }
                        return Err(ParsingErr("invalid ast~".into()))
                    }
                ),
                Consume(Token::CloseParen),
                first()
            ),
            second(),
        );
        return binding
    } 

    fn rhs_expr<'a>() -> impl DebugParseable<'a> {
        Match(
            Token::Integer,
            |lexr| 
                Ok(Expr::Lit(lexr.into()))
        )
    }

    fn decl_or_decl_assign_typed<'a>() -> impl DebugParseable<'a> {
        MatchSeq(
            Consume(Token::Var),
            MatchSeq(
                Self::parse_type_annotation(),
                Optional(
                    MatchSeq(
                        Consume(Token::Eq),
                        Self::rhs_expr(),
                        second()
                    ),
                    identity()
                ),
                |decl, rhs| {
                    if let Expr::Decl { typ, name, value:_  } = decl {
                        println!("AA {:?}", rhs);
                        return Ok(Expr::Decl { typ, name, value: rhs.into() });
                    }
                    return Err(ParsingErr(format!("expected declaration but got {:?}", decl)))
                }
            ),
            second()
        )
    }

    fn statement_parser<'a>() -> impl DebugParseable<'a> {
        Self::decl_or_decl_assign_typed()
    }

    fn statements_parser<'a>() -> impl DebugParseable<'a> {
        ZeroOrMore(
            MatchSeq(
                Self::statement_parser(),
                Consume(Token::Semicolon),
                first()
            ),
        collect())
    }


    fn function_parser<'a>() -> impl DebugParseable<'a> {
        let prototype = MatchSeq(
                Self::ident_parser(),
                Self::param_list(),
                |a, b| {
                    let mut list;
                    if let Expr::List(seq) = b {
                       list = seq.container.into_iter().map(|v| {
                            if let Expr::Decl { typ, name, value:_ }
                             = v.as_ref() {
                                return Some(Param{
                                    arg_name: name.clone(),
                                    typ: typ.clone(),
                                })
                            }
                           return None
                        }).filter(|x|x.is_some()).flatten().collect::<LinkedList<_>>();
                    } else {
                        return Err(ParsingErr("expected a argument list".into()));
                    }

                    if let Expr::Ident(src) = a {
                        return Ok(Expr::FuncDef { name: src, args: list, block: Seq::empty() })
                    }
                   return Err(ParsingErr("expected identifier found something else".into()))
                }
            );
        let func_parser = MatchSeq(
            Consume(Token::Func),
            MatchSeq(
                prototype,
                MatchSeq(
                    Consume(Token::OpenBrace),
                    MatchSeq(
                        Self::statements_parser(),
                        Consume(Token::CloseBrace),
                        first()
                    ),
                    second(),
                ),
                |a,b| {
                    if let Expr::FuncDef { name, args, block } = a {
                        if let Expr::List(seq) = b {
                            return Ok(Expr::FuncDef { name, args, block: seq })
                        }
                        return Err(ParsingErr(format!("unable to parse function {:?}", name.data)))
                    }
                    return Err(ParsingErr(format!("unable to parse function ")))
                }
            ),
            second()
        );
        func_parser
    }


    fn parse<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let parser = Self::function_parser();
        parser.consume_parse(lexr)

    }
    pub fn from(src: SrcFile) -> Self {
        Self { src }
    }
    pub fn ast(&self) -> PResult<Expr<'_>> {
        Self::parse(&mut self.src.lexer())
    }

}


#[cfg(test)]
mod tests {
    use crate::{srcfile::SrcFile, parsing::Parseable};

    use super::Ast;

    pub fn testsrc(src: &str) -> SrcFile {
        SrcFile {
            src: src.to_string(),
            path: "idc".into(),
        }
    }   
    #[test]
    pub fn test_param_list_parse() {
        let src = testsrc("func asd(a: )");
        let parser = Ast::function_parser();
        let result = parser.consume_parse(&mut src.lexer());
    }

}
