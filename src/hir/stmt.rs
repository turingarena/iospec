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
