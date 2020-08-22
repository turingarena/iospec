//! Post-analysis of HIR, common to running and code generation

use std::ops::Deref;

use crate::spec::hir::*;

#[derive(Debug, Clone)]
pub struct HAlloc {
    pub array: Rc<HNodeDef>,
    pub item_ty: Rc<HValTy>,
    pub size: Rc<HVal>,
}

/// Returns the variable declaration, if any, to be performed before defining this data node
pub fn node_decl(node: &Rc<HNodeDef>) -> Option<Rc<HVarDef>> {
    match &node.expr {
        HNodeDefExpr::Var { var } => Some(var.clone()),
        _ => None,
    }
}

/// Returns the array allocation, if any, to be performed before defining this data node
pub fn node_alloc(node: &Rc<HNodeDef>) -> Option<HAlloc> {
    match node.ty.deref() {
        HValTy::Array { item, range } => Some(HAlloc {
            array: node.clone(),
            item_ty: item.clone(),
            size: range.bound.val.clone(),
        }),
        _ => None,
    }
}
