use super::*;

#[derive(Debug, Clone)]
pub struct Block<'ast> {
    pub ast: &'ast ParsedBlock,
    pub stmts: Vec<Stmt<'ast>>,
}

impl<'ast> Compile<'ast, ParsedBlock> for Block<'ast> {
    fn compile(ast: &'ast ParsedBlock, scope: &Scope<'ast>) -> CompileResult<Self> {
        let mut stmts = vec![];
        let mut scope = scope.clone();

        for ast in ast.stmts.iter() {
            let stmt: Stmt = compile(ast, &scope)?;
            scope = stmt.extend_scope(scope);
            stmts.push(stmt);
        }

        Ok(Block { ast, stmts })
    }
}
