use super::*;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl {
        def: &'a Def<'a>,
    },
    Read {
        def: &'a Def<'a>,
    },
    Write {
        expr: &'a Expr<'a>,
    },
    Call {
        stmt: &'a Stmt<'a>,
    },
    For {
        range: &'a Range<'a>,
        body: IrBlock<'a>,
    },
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
            Stmt::Write { args, .. } => args.iter().map(|expr| IrInst::Write { expr }).collect(),
            Stmt::Call { return_value, .. } => vec![
                return_value
                    .iter()
                    .map(|def| IrInst::Decl { def })
                    .collect(),
                vec![IrInst::Call { stmt }],
            ]
            .into_iter()
            .flatten()
            .collect(),
            Stmt::For { range, body, .. } => vec![IrInst::For {
                range,
                body: body.build_ir(),
            }],
        }
    }
}
