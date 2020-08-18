//! Environment (as HIR) of an AST node.
//!
//! The *environment* of an AST node contains pointers to the information
//! defined *before* the AST node (in the tree post-order traversal),
//! which are relevant to interpret the semantics of the node itself.
//! This includes, e.g., the set of defined variables when the node is encountered.
//!
//! The environment is made of HIR nodes, and is used to transform an AST node directly into HIR
//! (possibly with new links to nodes in the environment).

use crate::hir::*;

#[derive(Debug, Clone)]
pub struct Env {
    pub refs: Vec<Rc<HVar>>,
    pub outer: Option<Box<Env>>,
    pub loc: Rc<HDefLoc>,
}

impl Env {
    pub fn resolve(self: &Self, ident: &HIdent) -> Option<Rc<HVar>> {
        self.refs
            .iter()
            .find(|r| r.ident.token == ident.token)
            .map(|r| r.clone())
            .or(self.outer.as_ref().and_then(|s| s.resolve(ident)))
    }
}

#[derive(Debug, Clone)]
pub struct HDefEnv {
    pub env: Env,
    pub atom_ty: Rc<HAtomTy>,
    pub ctx: Rc<HDefExprCtx>,
    pub loc: Rc<HDefLoc>,
}