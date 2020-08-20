//! Build MIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::mir::*;

pub fn build_mir(spec: &Rc<HSpec>) -> MSpec {
    MSpec {
        hir: spec.clone(),
        main: mir_insts(&spec.main),
    }
}

fn mir_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    match hir.deref() {
        HStmt::Block { stmts } => stmts.iter().flat_map(mir_insts).collect(),
        HStmt::Read { args, .. } => args
            .iter()
            .flat_map(|a| vec![MInst::Decl(a.node.root.clone()), MInst::Read(a.clone())])
            .collect(),
        HStmt::Write { args, .. } => args.iter().cloned().map(MInst::Write).collect(),
        HStmt::Call { fun, .. } => fun
            .ret
            .iter()
            .map(|r| MInst::Decl(r.node.root.clone()))
            .chain(std::iter::once(MInst::Call(fun.clone())))
            .collect(),
        HStmt::For { range, body, .. } => body
            .allocs()
            .iter()
            .flat_map(|node| match node.expr.deref() {
                HDataExpr::Subscript { array, index, .. } => Some(MInst::Alloc {
                    array: array.clone(),
                    ty: node.ty.clone(),
                    size: index.range.bound.val.clone(),
                }),
                _ => None,
            })
            .chain(std::iter::once(MInst::For {
                range: range.clone(),
                body: mir_insts(body),
            }))
            .collect(),
    }
}
