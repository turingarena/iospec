use super::*;

#[derive(Debug, Clone)]
pub enum CompiledExpr<'ast> {
    Var(CompiledExprVar<'ast>),
    Index(CompiledExprIndex<'ast>),
}

impl CompiledExpr<'_> {
    // TODO: array types
    pub fn ty(self: &Self) -> ScalarType {
        match self {
            CompiledExpr::Var(expr) => expr.decl.scalar_type_expr.ty,
            CompiledExpr::Index(expr) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompiledExprVar<'ast> {
    pub ast: &'ast ParsedExpr,
    pub decl: CompiledDecl<'ast>,
}

#[derive(Debug, Clone)]
pub struct CompiledExprIndex<'ast> {
    pub ast: &'ast ParsedExpr,
    pub array: Box<CompiledExpr<'ast>>,
    pub index: Box<CompiledExpr<'ast>>,
}

impl<'ast> Compile<'ast, ParsedExpr> for CompiledExpr<'ast> {
    fn compile(ast: &'ast ParsedExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedExpr::Var { ident } => CompiledExpr::Var(CompiledExprVar {
                ast,
                decl: match scope.resolve(&ident.sym) {
                    Some(NameResolution::Decl(decl)) => decl,
                    _ => panic!("undefined variable"),
                },
            }),
            ParsedExpr::Index { array, index, .. } => CompiledExpr::Index(CompiledExprIndex {
                ast,
                array: Box::new(compile(array.as_ref(), scope)?),
                index: Box::new(compile(index.as_ref(), scope)?),
            }),
        })
    }
}
