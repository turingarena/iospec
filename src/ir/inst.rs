use super::*;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl { def: &'a CompiledDef<'a> },
    Read { def: &'a CompiledDef<'a> },
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
                .flat_map(|def| vec![IrInst::Decl { def }, IrInst::Read { def }])
                .collect(),
            CompiledStmt::Write { args, .. } => {
                args.iter().map(|expr| IrInst::Write { expr }).collect()
            }
            CompiledStmt::Call { return_value, .. } => vec![
                return_value
                    .iter()
                    .map(|def| IrInst::Decl { def: def })
                    .collect(),
                vec![IrInst::Call { inner: stmt }],
            ]
            .into_iter()
            .flatten()
            .collect(),
            CompiledStmt::For { .. } => vec![IrInst::For { inner: stmt }],
        }
    }
}
