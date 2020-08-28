//! Environment (as HIR) of an AST node.
//!
//! The *environment* of an AST node contains pointers to the information
//! defined *before* the AST node (in the tree post-order traversal),
//! which are relevant to interpret the semantics of the node itself.
//! This includes, e.g., the set of defined variables when the node is encountered.
//!
//! The environment is made of HIR nodes, and is used to transform an AST node directly into HIR
//! (possibly with new links to nodes in the environment).

use crate::spec::hir::*;

use super::diagnostic::*;
use super::hir_err::HErr;

#[derive(Clone)]
pub struct Env {
    refs: Vec<Rc<HVar>>,
    outer: Option<Box<Env>>,

    pub loc: Rc<HDataLoc>,
}

impl Env {
    pub fn main() -> Self {
        Env {
            refs: Vec::new(),
            outer: None,
            loc: Rc::new(HDataLoc::Main),
        }
    }

    pub fn declare(self: &mut Self, var: &Rc<HVar>, diagnostics: &mut Vec<Diagnostic>) {
        match self.maybe_resolve(&var.name) {
            None => {
                self.refs.push(var.clone());
            }
            Some(old_var) => {
                diagnostics.push(Diagnostic::AlreadyDefinedVar {
                    new_var: var.clone(),
                    old_var,
                });
            }
        }
    }

    pub fn resolve(self: &Self, ident: &Rc<HName>, diagnostics: &mut Vec<Diagnostic>) -> Rc<HVar> {
        match self.maybe_resolve(ident) {
            Some(var) => var,
            None => {
                diagnostics.push(Diagnostic::UndefVar {
                    ident: ident.clone(),
                });
                Rc::new(HVar {
                    name: ident.clone(),
                    ty: HErr::err(),
                    expr: HErr::err(),
                })
            }
        }
    }

    fn maybe_resolve(self: &Self, ident: &Rc<HName>) -> Option<Rc<HVar>> {
        self.refs
            .iter()
            .find(|r| r.name.ident == ident.ident)
            .map(|r| r.clone())
            .or(self.outer.as_ref().and_then(|s| s.maybe_resolve(ident)))
    }

    pub fn for_body(self: &Self, range: Rc<HRange>) -> Self {
        Self {
            refs: vec![Rc::new(HVar {
                name: range.index.clone(),
                ty: range.bound.val.ty.clone(),
                expr: HVarExpr::Index {
                    range: range.clone(),
                },
            })],
            outer: Some(Box::new(self.clone())),
            loc: Rc::new(HDataLoc::For {
                range: range.clone(),
                parent: self.loc.clone(),
            }),
        }
    }
}

#[derive(Clone)]
pub struct HDefEnv {
    pub env: Env,
    pub loc: Rc<HDataLoc>,
    pub ty: Rc<HValTy>,
}

#[derive(Clone)]
pub enum HDataLoc {
    Main,
    For {
        range: Rc<HRange>,
        parent: Rc<HDataLoc>,
    },
}
