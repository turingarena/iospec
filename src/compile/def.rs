use super::*;

#[derive(Debug, Clone)]
pub struct CompiledDef<'ast> {
    pub ast: &'ast ParsedDef,
    pub name: &'ast str,
    // TODO: support array types
    pub scalar_type_expr: CompiledScalarTypeExpr<'ast>,
}

impl CompiledDef<'_> {
    pub fn expr(self: &Self) -> CompiledExpr {
        CompiledExpr::Var {
            ast: &self.ast.expr,
            def: self.clone(),
        }
    }
}

impl<'ast> Compile<'ast, ParsedDef> for CompiledDef<'ast> {
    fn compile(ast: &'ast ParsedDef, scope: &Scope<'ast>) -> CompileResult<Self> {
        let name = match &ast.expr {
            ParsedExpr::Var {
                ident: ParsedIdent { sym },
            } => sym,
            _ => Err("unsupported expression in declaration")?,
        };

        Ok(Self {
            ast,
            name,
            scalar_type_expr: compile(&ast.ty, &scope)?,
        })
    }
}
