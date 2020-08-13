use super::*;

#[derive(Debug, Clone)]
pub enum CompiledExpr<'ast> {
    Var {
        ast: &'ast ParsedExpr,
        def: CompiledDef<'ast>,
    },
    Index {
        ast: &'ast ParsedExpr,
        array: Box<CompiledExpr<'ast>>,
        index: Box<CompiledExpr<'ast>>,
    },
}

impl CompiledExpr<'_> {
    pub fn ty(self: &Self) -> VariableType {
        match self {
            CompiledExpr::Var { def, .. } => VariableType::Scalar { scalar_type: def.scalar_type_expr.ty.clone() },
            CompiledExpr::Index { array, .. } => VariableType::Array { array_type: Box::new(array.ty()),  },
        }
    }
}

impl<'ast> Compile<'ast, ParsedExpr> for CompiledExpr<'ast> {
    fn compile(ast: &'ast ParsedExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedExpr::Var { ident } => CompiledExpr::Var {
                ast,
                def: match scope.resolve(&ident.sym) {
                    Some(NameResolution::Def(def)) => def,
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
