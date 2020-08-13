use super::*;

#[derive(Debug, Clone)]
pub struct CompiledBlock<'ast> {
    pub ast: &'ast ParsedBlock,
    pub stmts: Vec<CompiledStmt<'ast>>,
}

impl<'ast> Compile<'ast, ParsedBlock> for CompiledBlock<'ast> {
    fn compile(ast: &'ast ParsedBlock, scope: &Scope<'ast>) -> CompileResult<Self> {
        let mut stmts = vec![];
        let mut scope = scope.clone();

        for ast in ast.stmts.iter() {
            let stmt: CompiledStmt = compile(ast, &scope)?;
            scope = stmt.extend_scope(scope);
            stmts.push(stmt);
        }

        Ok(CompiledBlock { ast, stmts })
    }
}
