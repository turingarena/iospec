use crate::hir::*;

#[derive(Debug, Clone)]
pub struct Env {
    pub refs: Vec<HN<HDecl>>,
    pub outer: Option<Box<Env>>,
    pub cons_path: ConsPath,
}

impl Env {
    pub fn resolve(self: &Self, ident: HN<HIdent>) -> Option<HN<HDecl>> {
        self.refs
            .iter()
            .find(|r| r.ident.token == ident.token)
            .map(|r| r.clone())
            .or(self.outer.as_ref().and_then(|s| s.resolve(ident)))
    }
}

#[derive(Debug, Clone)]
pub enum ConsPath {
    Root,
    For {
        range: HN<HRange>,
        parent: Box<ConsPath>,
    },
}
