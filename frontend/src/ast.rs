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
use std::hash::Hash;
use core::cmp::{Eq, Ord};        
#[derive(Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
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




#[derive(Debug, Clone, Hash,PartialEq, PartialOrd, Eq, Ord)]
pub struct Type<'source>(SrcInfo<'source>);

#[derive(Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Param<'source> {
    pub arg_name: SrcInfo<'source>,
    pub typ: Type<'source>,
}


#[derive(Clone, Hash)]
pub struct SrcInfo<'a> {
    span: Range<usize>,
    data: &'a str,
}
impl<'a> PartialOrd for SrcInfo<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data.partial_cmp(&other.data)
    }
}
impl<'a> Eq for SrcInfo<'a>{}

impl<'a> Ord for SrcInfo<'a>{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub const INT_TYPE: Type<'static> = Type(SrcInfo { span: 0..0, data: "int" });
pub const VOID_TYPE: Type<'static> = Type(SrcInfo { span: 0..0, data: "void" });

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

    pub fn placeholder() -> Self {
        Self { span: 0..0, data: "" }
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


        
#[derive(Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Fixity {
    Infix,
    Prefix,
}

// +(1,2) -> 

#[derive(Debug, Hash, PartialEq, PartialOrd, Eq, Ord)]
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
        args: LinkedList<Rc<Expr<'source>>>,
        fixity: Fixity,
        pure: bool
    },
    FuncDef {
        name: SrcInfo<'source>,
        args: LinkedList<Param<'source>>,
        block: Seq<'source>,
        return_type: Option<Type<'source>>,
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

    fn func_call<'a>() -> impl DebugParseable<'a> {
        MatchSeq(
            Self::ident_parser(),
            MatchSeq(
                Consume(Token::OpenParen),
                Consume(Token::CloseParen),
                empty_combine(),
            ),
            |a, b|{
                if let Expr::Ident(func_name) = a {
                    return Ok(Expr::FuncCall { op: func_name, args: LinkedList::new() , fixity: Fixity::Prefix, pure: false })
                }
                return Err(ParsingErr("expected identifier for function call".to_string()))
            }
        )
    }
    fn rhs_expr<'a>() -> impl DebugParseable<'a> {
        Either(
            Either(
                Self::func_call(),
                Self::ident_parser(),
                identity()
            ),
            Self::integer_parser(),
            identity()
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
                        return Ok(Expr::Decl { typ, name, value: rhs.into() });
                    }
                    return Err(ParsingErr(format!("expected declaration but got {:?}", decl)))
                }
            ),
            second()
        )
    }

    fn return_expr<'a>() -> impl DebugParseable<'a> {
        MatchSeq(
            Consume(Token::Return),
            Self::rhs_expr(),
            |_,ret_expr| {Ok(Expr::Return { expr: ret_expr.into() })}  
        )
    }

    fn statement_parser<'a>() -> impl DebugParseable<'a> {
        Either(
            Self::decl_or_decl_assign_typed(),
            Either(
                Self::return_expr(),
                Self::rhs_expr(),
                identity(),
            ),
            identity()
        )
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
                MatchSeq(
                    Self::param_list(),
                    Optional(
                        MatchSeq(
                            Consume(Token::Colon),
                            Self::ident_parser(),
                            |a,b| {
                                if let Expr::Ident(srcinfo) = b {
                                    return Ok(Expr::Type(Type(srcinfo)))
                                }
                                return Err(ParsingErr("expected type".to_string()))
                            }
                        ),
                        identity(),
                    ),
                    |param, ret_type| {
                        let rettype;
                        if let Expr::Type(ty) = ret_type {
                            rettype = Some(ty)
                        } else {
                            rettype = None
                        }

                        let mut list;
                        if let Expr::List(seq) = param {
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
                        Ok(Expr::FuncDef { name: SrcInfo::placeholder(), args: list , block: Seq::empty() , return_type: rettype})
                    }
                ),
                |ident, func_def|{
                    if let Expr::FuncDef { name: name_, args, block, return_type } = func_def {
                        if let Expr::Ident(name) = ident {
                            return Ok(Expr::FuncDef { name, args, block, return_type })
                        }
                        return Err(ParsingErr(format!("expected identifier found {:?}", ident)));
                    }
                    return Err(ParsingErr(format!("something went wrong parsing the param list and return type")));
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
                    if let Expr::FuncDef { name, args, block, return_type } = a {
                        if let Expr::List(seq) = b {
                            return Ok(Expr::FuncDef { name, args, block: seq , return_type: return_type})
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
        assert!(result.is_err())
    }

}
