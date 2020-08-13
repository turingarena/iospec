use super::*;

#[derive(Debug, Clone)]
pub struct Spec<'ast> {
    pub ast: &'ast ParsedSpec,
    pub main: Block<'ast>,
}

pub fn compile_spec(ast: &ParsedSpec) -> CompileResult<Spec> {
    Ok(Spec {
        ast,
        main: compile(&ast.main, &Scope::Root)?,
    })
}
