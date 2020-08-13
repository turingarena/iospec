use super::*;

#[derive(Debug, Copy, Clone)]
pub enum ScalarType {
    I32,
    N32,
    I64,
    N64,
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Scalar {
        scalar_type: ScalarType,
    },
    Array {
        array_type: Box<VariableType>,
    },
}

impl VariableType {
    pub fn scalar_type(self: &Self) -> ScalarType {
        match self {
            VariableType::Scalar { scalar_type } => *scalar_type,
            VariableType::Array { .. } => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompiledScalarTypeExpr<'ast> {
    pub ast: &'ast ParsedScalarTypeExpr,
    pub ty: ScalarType,
}

impl<'ast> Compile<'ast, ParsedScalarTypeExpr> for CompiledScalarTypeExpr<'ast> {
    fn compile(ast: &'ast ParsedScalarTypeExpr, _scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(Self {
            ast,
            ty: match ast.ident.sym.as_str() {
                "i32" => ScalarType::I32,
                "n32" => ScalarType::N32,
                "i64" => ScalarType::I64,
                "n64" => ScalarType::N64,
                _ => Err("invalid type")?,
            },
        })
    }
}
