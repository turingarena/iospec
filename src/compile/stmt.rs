use super::*;

#[derive(Debug, Clone)]
pub enum CompiledStmt<'ast> {
    Read(CompiledStmtRead<'ast>),
    Write(CompiledStmtWrite<'ast>),
    Call(CompiledStmtCall<'ast>),
    For(CompiledStmtFor<'ast>),
}

#[derive(Debug, Clone)]
pub struct CompiledStmtRead<'ast> {
    pub ast: &'ast ParsedStmt,
    pub args: Vec<CompiledDecl<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtWrite<'ast> {
    pub ast: &'ast ParsedStmt,
    pub args: Vec<CompiledExpr<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtCall<'ast> {
    pub ast: &'ast ParsedStmt,
    pub name: &'ast str,
    pub args: Vec<CompiledExpr<'ast>>,
    pub return_value: Option<CompiledDecl<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtFor<'ast> {
    pub ast: &'ast ParsedStmt,
    pub index_name: &'ast str,
    pub range: CompiledExpr<'ast>,
    pub body: CompiledBlock<'ast>,
}

impl<'ast> CompiledStmt<'ast> {
    pub fn extend_scope(self: &Self, scope: Scope<'ast>) -> Scope<'ast> {
        match self {
            CompiledStmt::Read(CompiledStmtRead { args, .. }) => {
                let mut current = scope;
                for decl in args {
                    current = Scope::Decl {
                        decl: decl.clone(),
                        parent: Box::new(current.clone()),
                    }
                }
                current
            }
            CompiledStmt::Call(CompiledStmtCall {
                                   return_value: Some(return_value),
                                   ..
                               }) => Scope::Decl {
                decl: return_value.clone(),
                parent: Box::new(scope.clone()),
            },
            _ => scope,
        }
    }
}

impl<'ast> Compile<'ast, ParsedStmt> for CompiledStmt<'ast> {
    fn compile(ast: &'ast ParsedStmt, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedStmt::Read { args, .. } => CompiledStmt::Read(CompiledStmtRead {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Write { args, .. } => CompiledStmt::Write(CompiledStmtWrite {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Call { name, args, return_value, .. } => CompiledStmt::Call(CompiledStmtCall {
                ast,
                name: &name.sym,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
                return_value: match &return_value {
                    Some((_, decl)) => Some(compile(decl, scope)?),
                    None => None,
                },
            }),
            ParsedStmt::For { index_name, range, body, .. } => CompiledStmt::For(CompiledStmtFor {
                ast,
                index_name: &index_name.sym,
                range: compile(range, &scope)?,
                // TODO: change the scope to include the index
                body: compile(body, &scope)?,
            }),
        })
    }
}
