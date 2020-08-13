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
    pub fn defs(self: &Self) -> Vec<Def<'ast>> {
        match self {
            Stmt::Read { args, .. } => args.to_vec(),
            Stmt::Call {
                return_value: Some(return_value),
                ..
            } => vec![return_value.clone()],
            // TODO: make into decls
            Stmt::For { body, range, .. } => body.defs(),
            _ => vec![],
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
            } => {
                let range = Range {
                    stmt_ast: ast,
                    index_name: &index_name.sym,
                    bound: compile(bound, &scope)?,
                };
                Stmt::For {
                    ast,
                    range: range.clone(),
                    body: compile(
                        body,
                        &Scope::For {
                            parent: Box::new(scope.clone()),
                            range,
                        },
                    )?,
                }
            }
        })
    }
}
