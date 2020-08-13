use super::*;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl { def: &'a Def<'a> },
    Read { def: &'a Def<'a> },
    Write { expr: &'a Expr<'a> },
    Call { inner: &'a Stmt<'a> },
    For { inner: &'a Stmt<'a> },
    // TODO: Alloc, Free, control structures
}

impl<'a> Stmt<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        let stmt = self;
        match self {
            Stmt::Read { args, .. } => args
                .iter()
                .flat_map(|def| vec![IrInst::Decl { def }, IrInst::Read { def }])
                .collect(),
            Stmt::Write { args, .. } => {
                args.iter().map(|expr| IrInst::Write { expr }).collect()
            }
            Stmt::Call { return_value, .. } => vec![
                return_value
                    .iter()
                    .map(|def| IrInst::Decl { def: def })
                    .collect(),
                vec![IrInst::Call { inner: stmt }],
            ]
            .into_iter()
            .flatten()
            .collect(),
            Stmt::For { .. } => vec![IrInst::For { inner: stmt }],
        }
    }
}
