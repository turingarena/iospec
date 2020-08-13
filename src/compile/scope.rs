use super::*;

#[derive(Debug, Clone)]
pub enum Scope<'ast> {
    Root,
    Def {
        def: Def<'ast>,
        parent: Box<Scope<'ast>>,
    },
    For {
        range: Range<'ast>,
        parent: Box<Scope<'ast>>,
    },
    Loop {
        // loop_stmt: &'ast StmtLoop,
        parent: Box<Scope<'ast>>,
    },
}

#[derive(Debug, Clone)]
pub enum NameResolution<'ast> {
    Def(Def<'ast>),
    Index(Range<'ast>),
}

impl<'ast> Scope<'ast> {
    pub fn resolve(self: &Self, name: &str) -> Option<NameResolution<'ast>> {
        match self {
            Scope::Def { def, parent } => {
                if def.name == name {
                    Some(NameResolution::Def(def.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            Scope::For { range, parent } => {
                if range.index_name == name {
                    Some(NameResolution::Index(range.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            _ => None,
        }
    }

    pub fn find_innermost_for(self: &Self) -> Option<(Range<'ast>, Scope<'ast>)> {
        match self {
            Scope::For { range, parent } => Some((range.clone(), parent.as_ref().clone())),
            Scope::Def { parent, .. } => parent.find_innermost_for(),
            _ => None,
        }
    }
}
