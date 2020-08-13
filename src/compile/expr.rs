use super::*;

#[derive(Debug, Clone)]
pub enum CompiledExpr<'ast> {
    Var {
        ast: &'ast ParsedExpr,
        decl: CompiledDecl<'ast>,
    },
    Index {
        ast: &'ast ParsedExpr,
        array: Box<CompiledExpr<'ast>>,
        index: Box<CompiledExpr<'ast>>,
    },
}

impl CompiledExpr<'_> {
    // TODO: array types
    pub fn ty(self: &Self) -> ScalarType {
        match self {
            CompiledExpr::Var { decl, .. } => decl.scalar_type_expr.ty.clone(),
            CompiledExpr::Index { .. } => unimplemented!(),
        }
    }
}

impl<'ast> Compile<'ast, ParsedExpr> for CompiledExpr<'ast> {
    fn compile(ast: &'ast ParsedExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedExpr::Var { ident } => CompiledExpr::Var {
                ast,
                decl: match scope.resolve(&ident.sym) {
                    Some(NameResolution::Decl(decl)) => decl,
                    _ => panic!("undefined variable"),
                },
            },
            ParsedExpr::Index { array, index, .. } => CompiledExpr::Index {
                ast,
                array: Box::new(compile(array.as_ref(), scope)?),
                index: Box::new(compile(index.as_ref(), scope)?),
            },
        })
    }
}
