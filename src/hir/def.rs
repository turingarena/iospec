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
