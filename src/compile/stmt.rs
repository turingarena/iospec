use super::*;

#[derive(Debug, Clone)]
pub enum Stmt<'ast> {
    Read {
        ast: &'ast ParsedStmt,
        args: Vec<Def<'ast>>,
    },
    Write {
        ast: &'ast ParsedStmt,
        args: Vec<Expr<'ast>>,
    },
    Call {
        ast: &'ast ParsedStmt,
        name: &'ast str,
        args: Vec<Expr<'ast>>,
        return_value: Option<Def<'ast>>,
    },
    For {
        ast: &'ast ParsedStmt,
        range: Range<'ast>,
        body: Block<'ast>,
    },
}

impl<'ast> Stmt<'ast> {
    pub fn extend_scope(self: &Self, scope: Scope<'ast>) -> Scope<'ast> {
        match self {
            Stmt::Read { args, .. } => {
                let mut current = scope;
                for def in args {
                    current = Scope::Def {
                        def: def.clone(),
                        parent: Box::new(current.clone()),
                    }
                }
                current
            }
            Stmt::Call {
                return_value: Some(return_value),
                ..
            } => Scope::Def {
                def: return_value.clone(),
                parent: Box::new(scope.clone()),
            },
            _ => scope,
        }
    }
}

impl<'ast> Compile<'ast, ParsedStmt> for Stmt<'ast> {
    fn compile(ast: &'ast ParsedStmt, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedStmt::Read { args, .. } => Stmt::Read {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            },
            ParsedStmt::Write { args, .. } => Stmt::Write {
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
            } => Stmt::Call {
                ast,
                name: &name.sym,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
                return_value: match &return_value {
                    Some((_, def)) => Some(compile(def, scope)?),
                    None => None,
                },
            },
            ParsedStmt::For {
                index_name,
                bound,
                body,
                ..
            } => Stmt::For {
                ast,
                range: Range {
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
