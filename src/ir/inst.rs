use super::*;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl { inner: &'a CompiledDecl<'a> },
    Read { decl: &'a CompiledDecl<'a> },
    Write { expr: &'a CompiledExpr<'a> },
    Call { inner: &'a CompiledStmt<'a> },
    For { inner: &'a CompiledStmt<'a> },
    // TODO: Alloc, Free, control structures
}

impl<'a> CompiledStmt<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        let stmt = self;
        match self {
            CompiledStmt::Read { args, .. } => args
                .iter()
                .flat_map(|decl| {
                    vec![
                        IrInst::Decl { inner: decl },
                        IrInst::Read { decl },
                    ]
                })
                .collect(),
            CompiledStmt::Write { args, .. } => args
                .iter()
                .map(|expr| IrInst::Write { expr })
                .collect(),
            CompiledStmt::Call { return_value, .. } => vec![
                return_value
                    .iter()
                    .map(|decl| IrInst::Decl { inner: decl })
                    .collect(),
                vec![IrInst::Call { inner: stmt }],
            ]
                .into_iter()
                .flatten()
                .collect(),
            CompiledStmt::For { .. } => vec![
                IrInst::For { inner: stmt },
            ],
        }
    }
}
