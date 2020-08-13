use super::*;

#[derive(Debug, Clone)]
pub enum CompiledStmt<'ast> {
    Read {
        ast: &'ast ParsedStmt,
        args: Vec<CompiledDecl<'ast>>,
    },
    Write {
        ast: &'ast ParsedStmt,
        args: Vec<CompiledExpr<'ast>>,
    },
    Call {
        ast: &'ast ParsedStmt,
        name: &'ast str,
        args: Vec<CompiledExpr<'ast>>,
        return_value: Option<CompiledDecl<'ast>>,
    },
    For {
        ast: &'ast ParsedStmt,
        range: CompiledRange<'ast>,
        body: CompiledBlock<'ast>,
    },
}

impl<'ast> CompiledStmt<'ast> {
    pub fn extend_scope(self: &Self, scope: Scope<'ast>) -> Scope<'ast> {
        match self {
            CompiledStmt::Read { args, .. } => {
                let mut current = scope;
                for decl in args {
                    current = Scope::Decl {
                        decl: decl.clone(),
                        parent: Box::new(current.clone()),
                    }
                }
                current
            }
            CompiledStmt::Call {
                return_value: Some(return_value),
                ..
            } => Scope::Decl {
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
            ParsedStmt::Read { args, .. } => CompiledStmt::Read {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            },
            ParsedStmt::Write { args, .. } => CompiledStmt::Write {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            },
            ParsedStmt::Call {
                name,
                args,
                return_value,
                ..
            } => CompiledStmt::Call {
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
            },
            ParsedStmt::For {
                index_name,
                bound,
                body,
                ..
            } => CompiledStmt::For {
                ast,
                range: CompiledRange {
                    stmt_ast: ast,
                    index_name: &index_name.sym,
                    bound: compile(bound, &scope)?,
                },
                // TODO: change the scope to include the index
                body: compile(body, &scope)?,
            },
        })
    }
}
