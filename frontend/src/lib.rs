mod parser;
#[macro_use]
extern crate lalrpop_util;



#[cfg(test)]
mod tests {
    use super::*;

    // lalrpop_util::lalrpop_mod!(pub calculator1); // synthesized by LALRPOP
    // #[test]
    // fn calculator1() {
    //     assert!(calculator1::TermParser::new().parse("22").is_ok());
    //     assert!(calculator1::TermParser::new().parse("(22)").is_err());;
    // }
    // #[test]
    // fn it_works() {
    //     let result = add(2, 2);
    //     assert_eq!(result, 4);
    // }
}
