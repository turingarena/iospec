//! Middle-level intermediate representation (MIR), used for running, and as a precursor of LIR.
//!
//! The MIR is a tree of executable instructions nodes, each wrapping a HIR node.
//! The MIR tree topology can be different from the AST and the HIR, as it contains extra nodes
//! for declaration of variables, allocation and de-allocation of arrays, and so on.

use crate::hir::*;

#[derive(Debug, Clone)]
pub struct MSpec {
    pub hir: Rc<HSpec>,
    pub main: MBlock,
}

pub type MBlock = Vec<MInst>;

#[derive(Debug, Clone)]
pub enum MInst {
    Decl(Rc<HVarDef>),
    Alloc {
        array: Rc<HNodeDef>,
        ty: Rc<HValTy>,
        size: Rc<HVal>,
    },
    Read(Rc<HAtomDef>),
    Write(Rc<HAtom>),
    Call(Rc<HFun>),
    For {
        range: Rc<HRange>,
        body: MBlock,
    },
}
