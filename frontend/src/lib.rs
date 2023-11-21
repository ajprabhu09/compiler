#![feature(let_chains)]
pub mod ast;
pub mod srcfile;
pub mod token;
pub mod pratt;
#[cfg(test)]
mod test {
    use std::fmt::format;

    use crate::{ast::Ast, srcfile::SrcFile, token::Token};
    use glob::glob;

    #[test]
    pub fn test_files() {
        let files = glob("./test_files/*.snake").expect("invalid path");
        for entry in glob("./test_files/*.snake").expect("Failed to read glob pattern") {
            match entry {
                Ok(path) => {
                    let fp = format!("{}", path.display());
                    let tokens = SrcFile::parse_file(&fp);
                    println!("{:?}", tokens.lexer().clone().collect::<Vec<_>>());
                    let ast = Ast::from(tokens);
                    println!("{:?}", ast.ast());
                }
                Err(e) => println!("No such file: {:?}", e),
            }
        }
    }
}
