//! Build MIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::mir::*;

pub fn build_mir(spec: &Rc<HSpec>) -> MSpec {
    MSpec {
        hir: spec.clone(),
        main: mir_stmt_insts(&spec.main),
    }
}

fn mir_stmt_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    match hir.expr.deref() {
        HStmtExpr::Block { stmts } => stmts.iter().flat_map(mir_stmt_insts).collect(),
        HStmtExpr::Read { args, .. } => args
            .iter()
            .flat_map(|a| {
                std::iter::empty()
                    .chain(mir_data_node_insts(&a.node))
                    .chain(std::iter::once(MInst::Read(a.clone())))
            })
            .collect(),
        HStmtExpr::Write { args, .. } => args.iter().cloned().map(MInst::Write).collect(),
        HStmtExpr::Call { fun, .. } => fun
            .ret
            .iter()
            .flat_map(|ret| mir_data_node_insts(&ret.node))
            .chain(std::iter::once(MInst::Call(fun.clone())))
            .collect(),
        HStmtExpr::For { range, body, .. } => std::iter::empty()
            .chain(hir.allocs.iter().flat_map(mir_data_node_insts))
            .chain(std::iter::once(MInst::For {
                range: range.clone(),
                body: mir_stmt_insts(body),
            }))
            .collect(),
    }
}

fn mir_data_node_insts(node: &Rc<HDataNode>) -> impl Iterator<Item = MInst> + '_ {
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
}
