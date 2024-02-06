#![feature(let_chains)]
#![feature(assert_matches)]
#![feature(trait_alias)]
pub mod ast;
pub mod srcfile;
pub mod token;
pub mod pratt;
pub mod parsing;
#[cfg(test)]
mod test {
    use std::{fmt::format, time::Instant};

    use crate::{ast::Ast, srcfile::SrcFile, token::Token};
    use glob::glob;

    #[test]
    pub fn test_files() {
        let files = glob("./test_files/*.snake").expect("invalid path");
        for entry in glob("./test_files/*.snake").expect("Failed to read glob pattern") {
            match entry {
                Ok(path) => {
                    let fp = format!("{}", path.display());
                    let now = Instant::now();
                    let tokens = SrcFile::parse_file(&fp);
                    // println!("{:?}", tokens.lexer().clone().collect::<Vec<_>>());
                    let ast = Ast::from(tokens);
                    let ast = ast.ast();  
                    println!("File {:?} took {:?}", fp, now.elapsed());

                    println!("AST:: {:#?}", ast);

                }
                Err(e) => println!("No such file: {:?}", e),
            }
        }
    }
}
