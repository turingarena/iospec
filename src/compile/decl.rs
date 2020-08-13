use super::*;

#[derive(Debug, Clone)]
pub struct CompiledDecl<'ast> {
    pub ast: &'ast ParsedDecl,
    pub name: &'ast str,
    // TODO: support array types
    pub scalar_type_expr: CompiledScalarTypeExpr<'ast>,
}

impl CompiledDecl<'_> {
    pub fn expr(self: &Self) -> CompiledExpr {
        CompiledExpr::Var(CompiledExprVar {
            ast: &self.ast.expr,
            decl: self.clone(),
        })
    }
}

impl<'ast> Compile<'ast, ParsedDecl> for CompiledDecl<'ast> {
    fn compile(ast: &'ast ParsedDecl, scope: &Scope<'ast>) -> CompileResult<Self> {
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
