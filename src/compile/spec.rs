use super::*;

#[derive(Debug, Clone)]
pub struct CompiledSpec<'ast> {
    pub ast: &'ast ParsedSpec,
    pub main: CompiledBlock<'ast>,
}

pub fn compile_spec(ast: &ParsedSpec) -> CompileResult<CompiledSpec> {
    Ok(CompiledSpec {
        ast,
        main: compile(&ast.main, &Scope::Root)?,
    })
}
