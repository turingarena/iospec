use super::*;

#[derive(Debug, Clone)]
pub struct Spec<'ast> {
    pub ast: &'ast ParsedSpec,
    pub main: Block<'ast>,
}
