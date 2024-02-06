use core::panicking::panic;
use std::{collections::{HashMap, BTreeMap}, rc::Rc};
use frontend::ast::{Type, Expr, INT_TYPE, VOID_TYPE};
use std::hash::Hash;

pub trait MapExpr<'a, B> {
    fn fmap(&mut self, mapper: fn(Self) -> B) -> B;
}

pub struct TypeChecker<'source> {
    context: BTreeMap<Rc<Expr<'source>>, Type<'source>>,
    expr: Rc<Expr<'source>>
}



trait TypeCheck<'a> {
    fn typecheck(self) -> TypeChecker<'a>;
}

impl<'a> TypeCheck<'a> for Expr<'a> {
    fn typecheck(self) -> TypeChecker<'a> {
        TypeChecker { context: BTreeMap::new(), expr: self.into() }
    }
}


impl<'a> TypeChecker<'a> {
    pub fn save(&mut self, expr: Rc<Expr<'a>>, type_: Type<'a>) {
        self.context.insert(expr.clone(), type_);
    }
    pub fn type_expr(&self, expr: Rc<Expr<'a>>) -> Option<Type<'a>> {
        self.context.get(&expr).cloned()
    }

    fn check(&mut self) -> Type<'a> {
        if self.context.contains_key(&self.expr) {
            return self.context.get(&self.expr).cloned().unwrap();
        }
        let res = match &*self.expr {
            Expr::Type(t) => {
                self.context.insert(self.expr.clone(), t.clone());
                *t
            },
            Expr::Ident(v) => {
                let res = self.type_expr(self.expr);
                if let Some(res) = res {
                    return res;
                }
                panic!("{:?} has not been typed but is used ", v);
            } ,
            Expr::Lit(_) => INT_TYPE.clone(), // TODO: this should be better
            Expr::Decl { typ, name, value } => {
                self.save(Expr::Ident(name.clone()).into(), typ.clone());
                VOID_TYPE.clone()
            },
            Expr::FuncCall { op, args, fixity, pure } => {
                let res = self.type_expr(self.expr);
                if let Some(res) = res {
                    return res;
                }
                panic!("{:?} has not been typed but is used ", args);
            },
            Expr::FuncDef { name, args, block, return_type } => {
                VOID_TYPE.clone()
            },
            Expr::List(seq) => {
                let mut ty = None;
                for x in seq.container.iter(){
                    if let Expr::Return { expr } = **x {
                        ty = self.type_expr(expr);
                    }                   
                }
                return ty;

            },
            Expr::Empty => VOID_TYPE.clone(),
            Expr::Return { expr } => expr.typecheck().check(),
        };
        self.context.insert(self.expr, res.clone());
        return res;

    }
}
