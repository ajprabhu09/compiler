use std::{ops, rc::Rc, fmt::{format, Debug}};

use logos::Lexer;

use crate::{
    ast::{Expr, SrcInfo, Seq},
    token::Token,
};

#[derive(Debug)]
pub struct ParsingErr(pub String); // TODO extend with good errors
pub type PResult<T> = Result<T, ParsingErr>;

pub fn unwrap_next_token(x: Option<Result<Token, ()>>) -> PResult<Token> {
    if let Some(Ok(token)) = x {
        return Ok(token);
    } else {
        return Err(ParsingErr("could not unrwap another next token".into()));
    }
}

pub fn peek_<'a>(lexr: &mut Lexer<'a, Token>) -> Option<PResult<Token>> {
    let res = (lexr.clone()).next(); // Is this zero cost ?
    res.map(|x| x.map_err(|_| ParsingErr("erro in peek_".into())))
}

pub fn peek<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Token> {
    let res = peek_(lexr);
    if let Some(Ok(v)) = res {
        return Ok(v);
    }
    return Err(ParsingErr("peek error".into()));
}

pub fn next<'a>(lexr: &mut Lexer<'a, Token>) -> Token {
    unwrap_next_token(lexr.next()).unwrap()
}

pub fn next_src<'a>(lexr: &mut Lexer<'a, Token>) -> (SrcInfo<'a>, Token) {
    let token = next(lexr);
    let src_info: SrcInfo<'a> = lexr.into();
    (src_info, token)
}

pub fn consume<'a>(lexr: &mut Lexer<'a, Token>) {
    next(lexr);
}
pub type ParseFn<'a> = fn(&mut Lexer<'a, Token>) -> PResult<Expr<'a>>;
pub type CombineExpFn<'a> = fn(Expr<'a>, Expr<'a>) -> PResult<Expr<'a>>;
pub type TfExpFn<'a> = fn(Expr<'a>) -> PResult<Expr<'a>>;

pub type ParseResult<'a> = PResult<Expr<'a>>;

pub trait Parseable<'a> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool;
    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>>;
    // fn parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>>;
}




#[derive(Clone, Copy, Debug)]
pub struct Match<'a>(pub Token, pub ParseFn<'a>);

impl<'a> Parseable<'a> for Match<'a>{
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        if let Ok(res) = peek(lexr) {
            return  res == self.0;         
        } 
        return false;
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        if !self.peek_match(lexr) {
            return Err(ParsingErr(format!("expected {:?} but got {:?}", self.0, peek(lexr))));
        }
        let _ = next(lexr);
        return self.1(lexr);
    }
}



#[derive(Clone, Debug)]
pub struct Consume(pub Token);

impl<'a> Parseable<'a> for Consume{
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        if let Ok(res) = peek(lexr) {
            return  res == self.0;         
        } 
        return false;
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        if !self.peek_match(lexr) {
            return Err(ParsingErr(format!("expected {:?} but got {:?}", self.0, peek(lexr))));
        }
        next(lexr);
        return Ok(Expr::Empty);
    }
}


#[derive(Debug)]
pub struct Either<'a, A: Parseable<'a>, B: Parseable<'a>>(pub A, pub B, pub TfExpFn<'a>);

impl<'a,A: Parseable<'a> + Debug, B: Parseable<'a> + Debug> Parseable<'a> for Either<'a, A, B> {
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        self.0.peek_match(lexr) || self.1.peek_match(lexr)
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        if self.0.peek_match(lexr) {
            return self.0.consume_parse(lexr)
        }
        if self.1.peek_match(lexr) {
            return self.1.consume_parse(lexr);
        }
        return Err(ParsingErr(format!("expected either ")));
    }
}
#[derive(Clone, Debug)]
pub struct MatchSeq<'a, A: Parseable<'a> + Debug, B: Parseable<'a> + Debug>(pub A, pub B, pub CombineExpFn<'a>);

impl<'a, A: Parseable<'a> + Debug, B: Parseable<'a> + Debug> Parseable<'a> for MatchSeq<'a, A, B> {
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        let mut lexr_clone = lexr.clone();
        let res = self.consume_parse(&mut lexr_clone);
        return res.is_ok();
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let ares = self.0.consume_parse(lexr)?;
        let bres = self.1.consume_parse(lexr)?;
        self.2(ares, bres)
    }
}
#[derive(Debug)]
pub struct ZeroOrMore<'a, T: Parseable<'a>>(pub T, pub CombineExpFn<'a>);
impl<'a, T: Parseable<'a> + Debug> Parseable<'a> for ZeroOrMore<'a, T> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool {
        true
    }

    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        let mut curr = Ok(Expr::List(Seq::empty()));
        loop {
            if !self.0.peek_match(lexr) {
                return curr;
            }
            let expr = self.0.consume_parse(lexr)?;
            if let Expr::Empty = expr {
                return curr;
            }
            curr = self.1(curr.unwrap(), expr)
        }
    }
}
#[derive(Debug)]
pub struct OneOrMore<'a, T: Parseable<'a>>(pub T, CombineExpFn<'a>);
impl<'a, T: Parseable<'a> + Debug> Parseable<'a> for OneOrMore<'a, T> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool {
        return self.0.peek_match(lexr);
    }

    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        if !self.0.peek_match(lexr){
            return Err(ParsingErr("expected one or more but got nothing".to_string()));
        }
        
        let mut curr = Ok(Expr::List(Seq::empty()));

        loop {
            let expr = self.0.consume_parse(lexr)?;
            if let Expr::Empty = expr {
                return curr;
            }
            curr = self.1(curr.unwrap(), expr)
        }

    }
}

pub fn empty_expr<'a>() -> ParseFn<'a> {
    |lex: &mut Lexer<'_, Token>|  { Ok(Expr::Empty)}
}

pub fn empty_combine<'a>() -> CombineExpFn<'a> {
    |a, b| Ok(Expr::Empty)
}
pub fn first<'a>() -> CombineExpFn<'a> {
    |a, b| Ok(a)
}

pub fn second<'a>() -> CombineExpFn<'a> {
    |a, b| Ok(b)
}

pub fn identity<'a>() -> TfExpFn<'a> {
    |a| Ok(a)
}

pub fn collect<'a>() -> CombineExpFn<'a> {
    |c,n| {
        if let Expr::List(seq) = c {
            return Ok(Expr::List(seq.push_back_expr(n)));
        }
        return Err(ParsingErr("unable to collect since first argument is not a list".into()))
    }
}

#[derive(Debug)]
pub struct Optional<'a, T: Parseable<'a>>(pub T, pub TfExpFn<'a>);
impl<'a, T: Parseable<'a> + Debug> Parseable<'a> for Optional<'a, T> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool {
        return true;
    }

    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        if !self.0.peek_match(lexr){
            return Ok(Expr::Empty);
        }
        let curr = self.0.consume_parse(lexr)?;
        self.1(curr)
    }
}

pub trait DebugParseable<'a> = Parseable<'a> + Debug;
#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use logos::{Lexer, Source};

    use crate::{
        ast::{Expr, SrcInfo, self},
        parsing::*,
        srcfile::SrcFile,
        token::{Token, self},
    };

    pub fn testsrc(src: &str) -> SrcFile {
        SrcFile {
            src: src.to_string(),
            path: "idc".into(),
        }
    }

    pub fn print_lexr<'a>(lexr: &mut Lexer<'a, Token>) {
        println!("{:?}", lexr.collect::<Vec<_>>())
    }
}
