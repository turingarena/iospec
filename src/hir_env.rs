//! Environment (as HIR) of an AST node.
//!
//! The *environment* of an AST node contains pointers to the information
//! defined *before* the AST node (in the tree post-order traversal),
//! which are relevant to interpret the semantics of the node itself.
//! This includes, e.g., the set of defined variables when the node is encountered.
//!
//! The environment is made of HIR nodes, and is used to transform an AST node directly into HIR
//! (possibly with new links to nodes in the environment).

use crate::diagnostic::*;
use crate::hir::*;
use crate::hir_err::HErr;

#[derive(Debug, Clone)]
pub struct Env {
    refs: Vec<Rc<HBinding>>,
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

    pub fn declare(self: &mut Self, var: &Rc<HBinding>, sess: &mut Sess) {
        match self.maybe_resolve(&var.name) {
            None => {
                self.refs.push(var.clone());
            }
            Some(old_var) => {
                sess.diagnostics.push(Diagnostic::AlreadyDefinedVar {
                    new_var: var.clone(),
                    old_var,
                });
            }
        }
    }

    pub fn resolve(self: &Self, ident: &Rc<HName>, sess: &mut Sess) -> Rc<HBinding> {
        match self.maybe_resolve(ident) {
            Some(var) => var,
            None => {
                sess.diagnostics.push(Diagnostic::UndefVar {
                    ident: ident.clone(),
                });
                Rc::new(HBinding {
                    name: ident.clone(),
                    ty: HErr::err(),
                    kind: HErr::err(),
                })
            }
        }
    }

    fn maybe_resolve(self: &Self, ident: &Rc<HName>) -> Option<Rc<HBinding>> {
        self.refs
            .iter()
            .find(|r| r.name.ident == ident.ident)
            .map(|r| r.clone())
            .or(self.outer.as_ref().and_then(|s| s.maybe_resolve(ident)))
    }

    pub fn for_body(self: &Self, range: Rc<HRange>) -> Self {
        Self {
            refs: vec![Rc::new(HBinding {
                name: range.index.clone(),
                ty: range.bound.val.ty.clone(),
                kind: Rc::new(HBindingKind::Index {
                    range: range.clone(),
                }),
            })],
            outer: Some(Box::new(self.clone())),
            loc: Rc::new(HDataLoc::For {
                range: range.clone(),
                parent: self.loc.clone(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HDefEnv {
    pub env: Env,
    pub loc: Rc<HDataLoc>,
    pub ty: Rc<HValTy>,
}

#[derive(Debug, Clone)]
pub enum HDataLoc {
    Main,
    For {
        range: Rc<HRange>,
        parent: Rc<HDataLoc>,
    },
}
