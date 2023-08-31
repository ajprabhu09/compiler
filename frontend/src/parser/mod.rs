mod ast;
mod function_parser;

use self::grammer::ExprParser;

lalrpop_util::lalrpop_mod!(pub grammer); // synthesized by LALRPOP
                                         // use crate::parser::ast::BinOp;

#[cfg(test)]
mod tests {
    use std::rc::Rc;


    use crate::parser::ast::{Expr, Term, Type, BlockNode};
    use Expr::*;
    use Type::*;
    use Term::*;
    use crate::parser::ast::BinOp::*;
    use self::grammer::ExprParser;

    lalrpop_util::lalrpop_mod!(pub grammer); // synthesized by LALRPOP
                                             // use crate::parser::ast::BinOp;

    #[test]
    pub fn test_number() {
        let result = grammer::TermParser::new().parse("1");
        assert!(result.is_ok());
    }
    #[test]
    pub fn test_bracket_number() {
        let result = grammer::ExprParser::new().parse("(1)").unwrap();
        assert_eq!(result, Expr::Term(1.into()))
    }

    #[test]
    pub fn test_bracket_number_multiple() {
        let result = grammer::ExprParser::new().parse("((1))").unwrap();
        assert_eq!(result, Expr::Term(1.into()))
    }

    #[test]
    pub fn test_err_bracket_number_multiple_unbalanced() {
        let result = grammer::TermParser::new().parse("((1)))");
        assert!(result.is_err());
    }

    #[test]
    pub fn test_variable() {
        let result = grammer::TermParser::new().parse("a").unwrap();
        assert_eq!(result, "a".into());
    }

    #[test]
    pub fn test_parse_types() {
        let result = grammer::TypeIdentParser::new().parse("Int32").unwrap();
        assert_eq!(result, Type::Int32);
        let result = grammer::TypeIdentParser::new().parse("Char").unwrap();
        assert_eq!(result, Type::Char);
        let result = grammer::TypeIdentParser::new().parse("Bool").unwrap();
        assert_eq!(result, Type::Bool);
        let result = grammer::TypeIdentParser::new().parse("Int64");
        assert!(result.is_err());
    }

    #[test]
    pub fn test_parse_expr() {
        let a = ExprParser::new().parse("1 + 2").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "+".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: Expr::Term(2.into()).into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr2() {
        let a = ExprParser::new().parse("1 + a").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "+".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: Expr::Term("a".into()).into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr_multi() {
        let a = ExprParser::new().parse("1 + a + 1").unwrap();
        let b = ExprParser::new().parse("a + 1").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "+".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: b.into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr_multi2() {
        let a = ExprParser::new().parse("1 - a + 1").unwrap();
        let b = ExprParser::new().parse("a + 1").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "-".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: b.into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr_multi_right_assoc() {
        let a = ExprParser::new().parse("1 - a * 1").unwrap();
        let b = ExprParser::new().parse("a * 1").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "-".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: b.into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr_multi_right_assoc2() {
        let a = ExprParser::new().parse("1 / a / 1").unwrap();
        let b = ExprParser::new().parse("a / 1").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "/".into(),
                lhs: Expr::Term(1.into()).into(),
                rhs: b.into()
            }
        )
    }

    #[test]
    pub fn test_parse_expr_multi_prattness() {
        let a = ExprParser::new().parse("1 / a - 1 ").unwrap();
        let b = ExprParser::new().parse("1").unwrap();
        assert_eq!(
            a,
            Expr::BinExpr {
                op: "-".into(),
                lhs: Expr::BinExpr {
                    op: "/".into(),
                    lhs: Rc::new(1.into()),
                    rhs: Rc::new("a".into()),
                }
                .into(),
                rhs: b.into()
            }
        )
    }
    #[test]
    pub fn test_parse_expr_multi_prattness2() {
        let a = ExprParser::new().parse("1 /* lol */ / a - 1 + 1 + 1 // this is a test").unwrap();
        println!("{:#?}", a);
        // todo!("Write asserts here")
    }
    
    // #[test]
    pub fn this_is_hardcore_man() {
        let test = "1 + ".repeat(1000000).to_string() + "1";
        stacker::grow(1024 * 1024 * 1024, || assert!(test.parse::<Expr>().is_ok()))
    }

    #[test]
    pub fn test_decl_parsing() {
        let test = ExprParser::new().parse("var x = 1");
        println!("{:#?}", test);
    }

    #[test]
    pub fn test_block_parsing() {
        let test: Expr = "{
            a + 1
            a + 2
            a + 3
        }".parse().unwrap();
        let ans = BlockExpr {
            _type: UnTyped,
            start: Some(
                BlockNode {
                    curr: BinExpr {
                        op: Plus,
                        lhs: Term(
                            Var(
                                "a".to_string(),
                            ),
                        ).into(),
                        rhs: Term(
                            I32(
                                1,
                            ),
                        ).into(),
                    }.into(),
                    succ: Some(
                        BlockNode {
                            curr: BinExpr {
                                op: Plus,
                                lhs: Term(
                                    Var(
                                        "a".to_string(),
                                    ),
                                ).into(),
                                rhs: Term(
                                    I32(
                                        2,
                                    ),
                                ).into(),
                            }.into(),
                            succ: Some(
                                BlockNode {
                                    curr: BinExpr {
                                        op: Plus,
                                        lhs: Term(
                                            Var(
                                                "a".into(),
                                            ),
                                        ).into(),
                                        rhs: Term(
                                            I32(
                                                3,
                                            ),
                                        ).into(),
                                    }.into(),
                                    succ: None,
                                }.into(),
                            ),
                        }.into(),
                    ),
                }.into(),
            ),
        };
        assert_eq!(test, ans)
    }
   
    #[test]
    pub fn test_block_parsing_fail() {
        let test: Result<Expr, _> = "{
            val a = 1
            1a2+ 2a + 3
        }".parse();

        println!("{:#?}", test);

        assert!(test.is_err());
    }

}
