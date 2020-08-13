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

            for def in stmt.defs() {
                scope = Scope::Def {
                    def,
                    parent: Box::new(scope),
                };
            }
            stmts.push(stmt);
        }

        Ok(Block { ast, stmts })
    }
}

impl<'ast> Block<'ast> {
    pub fn defs(self: &Self) -> Vec<Def<'ast>> {
        self.stmts.iter().flat_map(Stmt::defs).collect()
    }
}
