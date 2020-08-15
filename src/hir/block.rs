use super::*;

#[derive(Debug, Clone)]
pub struct Block<'ast> {
    pub ast: &'ast ParsedBlock,
    pub stmts: Vec<Stmt<'ast>>,
}

impl<'ast> Block<'ast> {
    pub fn defs(self: &Self) -> Vec<Def<'ast>> {
        self.stmts.iter().flat_map(Stmt::defs).collect()
    }
}
