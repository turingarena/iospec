use super::*;

#[derive(Debug, Clone)]
pub struct CompiledRange<'ast> {
    pub stmt_ast: &'ast ParsedStmt,
    pub index_name: &'ast str,
    pub bound: CompiledExpr<'ast>,
}
