use super::*;

#[derive(Debug, Clone)]
pub struct Def<'ast> {
    pub ast: &'ast ParsedDef,
    pub name: &'ast str,
    pub value_type_expr: ScalarTypeExpr<'ast>,
    pub variable_type: VariableType<'ast>,
}

impl Def<'_> {
    pub fn expr(self: &Self) -> Expr {
        self.variable_type
            .compile_def_expr(self.clone(), &self.ast.expr)
    }
}

impl<'ast> Compile<'ast, ParsedDef> for Def<'ast> {
    fn compile(ast: &'ast ParsedDef, scope: &Scope<'ast>) -> CompileResult<Self> {
        let value_type_expr: ScalarTypeExpr = compile(&ast.ty, &scope)?;

        let mut variable_type = VariableType::Scalar {
            expr: value_type_expr.clone(),
        };

        let mut expr = &ast.expr;
        let mut scope = scope.clone();

        while let ParsedExpr::Subscript { array, index, .. } = expr {
            let index_name = if let ParsedExpr::Ref { ident } = index.as_ref() {
                &ident.sym
            } else {
                Err("invalid index in definition: only `for` indexes are supported ")?
            };

            if let Some((range, outer_scope)) = scope.find_innermost_for() {
                scope = outer_scope;
                if range.index_name == index_name {
                    variable_type = VariableType::Array {
                        item: Box::new(variable_type),
                        range: Box::new(range),
                    }
                } else {
                    Err("invalid index in definition, only the innermost for index is valid")?
                }
            } else {
                Err("more indices that for's")?
            }
            expr = array;
        }

        let name = match expr {
            ParsedExpr::Ref {
                ident: ParsedIdent { sym },
            } => sym,
            _ => Err("unsupported expression in declaration")?,
        };

        Ok(Self {
            ast,
            name,
            value_type_expr,
            variable_type,
        })
    }
}
