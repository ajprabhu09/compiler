use std::fs;

use logos::{Lexer, Logos};

use crate::token::Token;

pub struct SrcFile {
    pub src: String,
    pub path: String,
}

impl SrcFile {
    pub fn lexer(&self) -> Lexer<Token> {
        Token::lexer(&self.src)
    }

    pub fn parse_file(file_path: &str) -> SrcFile {
        let err_mesg = &format!("error: no such file {}", file_path);
        let fstring = fs::read_to_string(file_path).expect(&err_mesg);
        SrcFile {
            src: fstring,
            path: file_path.to_string(),
        }
    }
}
