use super::*;

#[derive(Debug, Clone)]
pub enum Expr<'ast> {
    Var {
        ast: &'ast ParsedExpr,
        def: Def<'ast>,
    },
    Index {
        ast: &'ast ParsedExpr,
        array: Box<Expr<'ast>>,
        index: Box<Expr<'ast>>,
    },
}

impl Expr<'_> {
    pub fn ty(self: &Self) -> VariableType {
        match self {
            Expr::Var { def, .. } => VariableType::Scalar { scalar_type: def.scalar_type_expr.ty.clone() },
            Expr::Index { array, .. } => VariableType::Array { array_type: Box::new(array.ty()),  },
        }
    }
}

impl<'ast> Compile<'ast, ParsedExpr> for Expr<'ast> {
    fn compile(ast: &'ast ParsedExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedExpr::Var { ident } => Expr::Var {
                ast,
                def: match scope.resolve(&ident.sym) {
                    Some(NameResolution::Def(def)) => def,
                    _ => panic!("undefined variable"),
                },
            },
            ParsedExpr::Index { array, index, .. } => Expr::Index {
                ast,
                array: Box::new(compile(array.as_ref(), scope)?),
                index: Box::new(compile(index.as_ref(), scope)?),
            },
        })
    }
}
