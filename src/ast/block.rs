use super::*;

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: Vec<ParsedStmt>,
}
