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

fn mir_stmt_insts(hir: &Rc<HStep>) -> Vec<MInst> {
    match &hir.expr {
        HStepExpr::Seq { steps } => steps.iter().flat_map(mir_stmt_insts).collect(),
        HStepExpr::Read { args, .. } => args
            .iter()
            .flat_map(|a| {
                std::iter::empty()
                    .chain(mir_data_node_insts(&a.node))
                    .chain(std::iter::once(MInst::Read(a.clone())))
            })
            .collect(),
        HStepExpr::Write { args, .. } => args.iter().cloned().map(MInst::Write).collect(),
        HStepExpr::Call { fun, .. } => fun
            .ret
            .iter()
            .flat_map(|ret| mir_data_node_insts(&ret.node))
            .chain(std::iter::once(MInst::Call(fun.clone())))
            .collect(),
        HStepExpr::For { range, body, .. } => std::iter::empty()
            .chain(hir.nodes.iter().flat_map(mir_data_node_insts))
            .chain(std::iter::once(MInst::For {
                range: range.clone(),
                body: mir_stmt_insts(body),
            }))
            .collect(),
    }
}

fn mir_data_node_insts(node: &Rc<HNodeDef>) -> impl Iterator<Item = MInst> + '_ {
    std::iter::empty()
        .chain(match &node.expr {
            HNodeDefExpr::Var { var } => Some(MInst::Decl(var.clone())),
            _ => None,
        })
        .chain(match node.ty.deref() {
            HValTy::Array { item, range } => Some(MInst::Alloc {
                array: node.clone(),
                ty: item.clone(),
                size: range.bound.val.clone(),
            }),
            _ => None,
        })
}
