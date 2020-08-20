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
            .flat_map(|a| {
                std::iter::empty()
                    .chain(a.node.var.iter().cloned().map(MInst::Decl))
                    .chain(std::iter::once(MInst::Read(a.clone())))
            })
            .collect(),
        HStmt::Write { args, .. } => args.iter().cloned().map(MInst::Write).collect(),
        HStmt::Call { fun, .. } => fun
            .ret
            .iter()
            .map(|r| MInst::Decl(r.node.root_var.clone()))
            .chain(std::iter::once(MInst::Call(fun.clone())))
            .collect(),
        HStmt::For { range, body, .. } => std::iter::empty()
            .chain(hir.allocs().iter().flat_map(|node| {
                std::iter::empty()
                    .chain(node.var.iter().cloned().map(MInst::Decl))
                    .chain(match node.ty.deref() {
                        HValTy::Array { item, range } => Some(MInst::Alloc {
                            array: node.clone(),
                            ty: item.clone(),
                            size: range.bound.val.clone(),
                        }),
                        _ => None,
                    })
            }))
            .chain(std::iter::once(MInst::For {
                range: range.clone(),
                body: mir_insts(body),
            }))
            .collect(),
    }
}
