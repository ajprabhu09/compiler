mod function_parser;
mod ast;




#[cfg(test)]
mod tests {
    use crate::parser::ast::{Type, Expr, Term};

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

        let result = grammer::TypeParser::new().parse("Int32").unwrap();
        assert_eq!(result, Type::Int32);
        let result = grammer::TypeParser::new().parse(" Char ").unwrap();
        assert_eq!(result, Type::Char);
        let result = grammer::TypeParser::new().parse("Bool ").unwrap();
        assert_eq!(result, Type::Bool);
        let result = grammer::TypeParser::new().parse("Int64");
        assert!(result.is_err());
    }

    #[test]
    pub fn test_parse_expr() {
        let a = ExprParser::new().parse("1 + 2").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "+".into(), lhs: Expr::Term(1.into()).into(), rhs: Expr::Term(2.into()).into() })
    }

    #[test]
    pub fn test_parse_expr2() {
        let a = ExprParser::new().parse("1 + a").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "+".into(), lhs: Expr::Term(1.into()).into(), rhs: Expr::Term("a".into()).into() })
    }


    #[test]
    pub fn test_parse_expr_multi() {
        let a = ExprParser::new().parse("1 + a + 1").unwrap();
        let b = ExprParser::new().parse("a + 1").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "+".into(), lhs: Expr::Term(1.into()).into(), rhs: b.into() })
    }


    #[test]
    pub fn test_parse_expr_multi2() {
        let a = ExprParser::new().parse("1 - a + 1").unwrap();
        let b = ExprParser::new().parse("a + 1").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "-".into(), lhs: Expr::Term(1.into()).into(), rhs: b.into() })
    }

    #[test]
    pub fn test_parse_expr_multi_right_assoc() {
        let a = ExprParser::new().parse("1 - a * 1").unwrap();
        let b = ExprParser::new().parse("a * 1").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "-".into(), lhs: Expr::Term(1.into()).into(), rhs: b.into() })
    }

    #[test]
    pub fn test_parse_expr_multi_right_assoc2() {
        let a = ExprParser::new().parse("1 - a / 1").unwrap();
        let b = ExprParser::new().parse("a / 1").unwrap();
        assert_eq!(a, Expr::BinExpr { op: "-".into(), lhs: Expr::Term(1.into()).into(), rhs: b.into() })
    }






}
