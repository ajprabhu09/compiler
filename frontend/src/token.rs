use std::{clone, fs};

use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
    // Or regular expressions.
    #[token(";")]
    Semicolon,

    #[regex("[0-9]+")]
    Integer,

    #[token("func")]
    Func,

    #[token("struct")]
    Struct,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    #[token("=")]
    Eq,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    ForwardSlash,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("return")]
    Return,

    #[token("var")]
    Var,

    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Ident,
}
