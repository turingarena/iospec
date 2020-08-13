use super::*;

#[derive(Debug, Clone)]
pub struct Def<'ast> {
    pub ast: &'ast ParsedDef,
    pub name: &'ast str,
    // TODO: support array types
    pub scalar_type_expr: ScalarTypeExpr<'ast>,
}

impl Def<'_> {
    pub fn expr(self: &Self) -> Expr {
        Expr::Var {
            ast: &self.ast.expr,
            def: self.clone(),
        }
    }
}

impl<'ast> Compile<'ast, ParsedDef> for Def<'ast> {
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
