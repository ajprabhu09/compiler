use std::ops;

use logos::Lexer;

use crate::{
    ast::{Expr, SrcInfo},
    token::Token,
};

#[derive(Debug)]
pub struct ParsingErr; // TODO extend with good errors
pub type PResult<T> = Result<T, ParsingErr>;

pub fn unwrap_next_token(x: Option<Result<Token, ()>>) -> PResult<Token> {
    if let Some(Ok(token)) = x {
        return Ok(token);
    } else {
        return Err(ParsingErr);
    }
}

pub fn peek_<'a>(lexr: &mut Lexer<'a, Token>) -> Option<PResult<Token>> {
    let res = (lexr.clone()).next(); // Is this zero cost ?
    res.map(|x| x.map_err(|_| ParsingErr))
}

pub fn peek<'a>(lexr: &mut Lexer<'a, Token>) -> PResult<Token> {
    let res = peek_(lexr);
    if let Some(Ok(v)) = res {
        return Ok(v);
    }
    return Err(ParsingErr);
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

pub type ParseResult<'a> = PResult<Expr<'a>>;

pub trait Parseable<'a> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool;
    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>>;
    fn parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>>;
}




#[derive(Clone, Copy)]
pub struct Match<'a>(pub Token, pub ParseFn<'a>);

impl<'a> Parseable<'a> for Match<'a>{
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        if let Ok(res) = peek(lexr) {
            return  res == self.0;         
        } 
        return false;
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        if self.peek_match(lexr) {
            let _ = next(lexr);
            return Some(self.parse(lexr));
        }
        return None
    }

    fn parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        self.1(lexr)
    }
    
}



#[derive(Clone, Copy)]
pub struct Consume(pub Token);

impl<'a> Parseable<'a> for Consume{
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        if let Ok(res) = peek(lexr) {
            return  res == self.0;         
        } 
        return false;
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        if self.peek_match(lexr) {
            let _ = next(lexr);
            return Some(Ok(Expr::Empty));
        }
        return None
    }

    fn parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        Ok(Expr::Empty)
    }
    
}

pub struct Either<'a, 'b>(&'b dyn Parseable<'a>,&'b dyn Parseable<'a>);

impl<'a,'b> Parseable<'a> for Either<'a, 'b> {
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        self.0.peek_match(lexr) || self.1.peek_match(lexr)
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        if self.0.peek_match(lexr) {
            return self.0.consume_parse(lexr)
        }
        if self.1.peek_match(lexr) {
            return self.1.consume_parse(lexr);
        }
        return None;
    }

    fn parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        self.consume_parse(lexr).unwrap()
    }
}
#[derive(Clone)]
pub struct Seq<'a, 'b>(pub &'b dyn Parseable<'a>, pub &'b dyn Parseable<'a>,pub CombineExpFn<'a>);

impl<'a, 'b> Parseable<'a> for Seq<'a, 'b> {
    fn peek_match(& self, lexr: &mut Lexer<'a, Token>) -> bool {
        return self.0.peek_match(lexr);
    }

    fn consume_parse(& self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        let ares = self.0.consume_parse(lexr)?;
        if let Err(_) = ares {
            return Some(ares);
        }
        let ares = ares.unwrap();
        let bres = self.1.consume_parse(lexr)?;
        if let Err(_) = bres {
            return Some(bres);
        }
        let bres = bres.unwrap();
        Some(self.2(ares, bres))
    }

    fn parse(& self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        self.consume_parse(lexr).unwrap()
    }
}

pub struct ZeroOrMore<'a, 'b>(pub &'b dyn Parseable<'a>,pub CombineExpFn<'a>);
impl<'a, 'b> Parseable<'a> for ZeroOrMore<'a, 'b> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool {
        true
    }

    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        let mut curr = Ok(Expr::Empty);

        loop {
            let expr = self.0.consume_parse(lexr);
            if expr.is_none() {
                return Some(curr);
            }
            let expr = expr.unwrap();
            if let Err(_)  = expr {
                return Some(expr)
            }
            curr = self.1(curr.unwrap(), expr.unwrap())
        }

    }

    fn parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        self.consume_parse(lexr).unwrap()
    }
}

pub struct OneOrMore<'a, 'b>(&'b dyn Parseable<'a>, CombineExpFn<'a>);
impl<'a, 'b> Parseable<'a> for OneOrMore<'a, 'b> {
    fn peek_match(&self, lexr: &mut Lexer<'a, Token>) -> bool {
        return self.0.peek_match(lexr);
    }

    fn consume_parse(&self, lexr: &mut Lexer<'a, Token>) -> Option<PResult<Expr<'a>>> {
        if !self.0.peek_match(lexr){
            return None;
        }
        
        let mut curr = self.0.consume_parse(lexr)?;

        loop {
            let expr = self.0.consume_parse(lexr);
            if expr.is_none() {
                return Some(curr);
            }
            let expr = expr.unwrap();
            if let Err(_)  = expr {
                return Some(expr)
            }
            curr = self.1(curr.unwrap(), expr.unwrap())
        }

    }

    fn parse(&self, lexr: &mut Lexer<'a, Token>) -> PResult<Expr<'a>> {
        self.consume_parse(lexr).unwrap()
    }
}



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

    #[test]
    pub fn test_match() {
        let mut p = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let src = testsrc("abcd");
        let res = p.consume_parse(&mut src.lexer());
        assert!(res.is_some());
        let res = res.unwrap();
        assert!(res.is_ok());
        assert_matches!(res, Ok(Expr::Ident(_)));
    }

    #[test]
    pub fn test_either() {
        let src = testsrc("=");

        let mut ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let mut int_lit = Match(Token::Integer, |lexr| {
            Ok(Expr::Lit(lexr.into()))
        });
        let mut either = Either(&mut ident, &mut int_lit);
        let mut eq = Match(Token::Eq, |lexr| {
            Ok(Expr::Empty)
        });
        let mut either2 = Either(&mut either, &mut eq);
       
        let res = either2.consume_parse(&mut src.lexer());
        
        println!("{:?}", res);
    }

    #[test]
    pub fn test_seq() {
        let src = testsrc("abc 123");

        let mut ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let mut id1 = ident.clone();
        let mut  id2 = ident.clone();
            
        let mut int_lit = Match(Token::Integer, |lexr| {
            Ok(Expr::Lit(lexr.into()))
        });

        let mut seq = Seq(&ident, &int_lit, |a,b|{
            Ok(Expr::Block(ast::Seq::empty().push_expr(a).push_expr(b)))
        });

        println!("seq {:?}", seq.consume_parse(&mut src.lexer()));

    }

    #[test]
    pub fn test_zero_or_more() {

        let src = testsrc("abc abc abc abc");
        let ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let multiple = ZeroOrMore(&ident, |a,b| {
            Ok(Expr::Block(ast::Seq::empty().push_expr(a).push_expr(b)))
        });
        let res = multiple.consume_parse(&mut src.lexer());

        println!("zero or more1 : {:?}", res);

    }

    #[test]
    pub fn test_zero_or_more_zero() {
        let src = testsrc("");
        let ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let multiple = ZeroOrMore(&ident, |a,b| {
            Ok(Expr::Block(ast::Seq::empty().push_expr(a).push_expr(b)))
        });
        let res = multiple.consume_parse(&mut src.lexer());
        println!("zero or more 2: {:?}", res);
    }
    #[test]
    pub fn test_one_more() {
        let src = testsrc("abc abc");
        let ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let multiple = OneOrMore(&ident, |a,b| {
            Ok(Expr::Block(ast::Seq::empty().push_expr(a).push_expr(b)))
        });
        let res = multiple.consume_parse(&mut src.lexer());
        println!("one or more: {:?}", res);
    }

    #[test]
    pub fn test_one_more_zero() {
        let src = testsrc("");
        let ident = Match(Token::Ident, |lexr| {
            Ok(Expr::Ident(lexr.into()))
        });
        let multiple = OneOrMore(&ident, |a,b| {
            Ok(Expr::Block(ast::Seq::empty().push_expr(a).push_expr(b)))
        });
        let res = multiple.consume_parse(&mut src.lexer());
        println!("one or more 2: {:?}", res);
    }
}
