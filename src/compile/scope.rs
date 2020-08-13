use super::*;

#[derive(Debug, Clone)]
pub enum Scope<'ast> {
    Root,
    Decl {
        decl: CompiledDecl<'ast>,
        parent: Box<Scope<'ast>>,
    },
    For {
        range: CompiledRange<'ast>,
        parent: Box<Scope<'ast>>,
    },
    Loop {
        // loop_stmt: &'ast StmtLoop,
        parent: Box<Scope<'ast>>,
    },
}

#[derive(Debug, Clone)]
pub enum NameResolution<'ast> {
    Decl(CompiledDecl<'ast>),
    Range(CompiledRange<'ast>),
}

impl<'ast> Scope<'ast> {
    pub fn resolve(self: &Self, name: &str) -> Option<NameResolution<'ast>> {
        match self {
            Scope::Decl { decl, parent } => {
                if decl.name == name {
                    Some(NameResolution::Decl(decl.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            Scope::For { range, parent } => {
                if range.index_name == name {
                    Some(NameResolution::Range(range.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            _ => None,
        }
    }
}
