#[cfg(test)]
mod tests {
    lalrpop_util::lalrpop_mod!(pub grammer); // synthesized by LALRPOP
    use crate::parser::ast::BinOp;
}
