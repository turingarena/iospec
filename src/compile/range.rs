use super::*;

#[derive(Debug, Clone)]
pub struct Range<'ast> {
    pub stmt_ast: &'ast ParsedStmt,
    pub index_name: &'ast str,
    pub bound: Expr<'ast>,
}
