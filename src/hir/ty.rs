use super::*;

#[derive(Debug, Copy, Clone)]
pub enum ScalarType {
    I32,
    N32,
    I64,
    N64,
}

#[derive(Debug, Clone)]
pub enum VariableType<'ast> {
    Index {
        range: Box<Range<'ast>>,
    },
    Scalar {
        expr: ScalarTypeExpr<'ast>,
    },
    Array {
        item: Box<VariableType<'ast>>,
        range: Box<Range<'ast>>,
    },
}

impl<'ast> VariableType<'ast> {
    pub fn scalar_type(self: &Self) -> ScalarType {
        match self {
            VariableType::Index { .. } => ScalarType::N32,
            VariableType::Scalar { expr } => expr.ty,
            VariableType::Array { .. } => unreachable!(),
        }
    }

    pub fn inner_scalar_type(self: &Self) -> ScalarType {
        match self {
            VariableType::Array { item, .. } => item.inner_scalar_type(),
            _ => self.scalar_type(),
        }
    }

    pub fn compile_def_expr(self: &Self, def: Def<'ast>, expr: &'ast ParsedExpr) -> Expr {
        match self {
            VariableType::Scalar { .. } => Expr::VarRef { ast: expr, def },
            VariableType::Array { item, range } => {
                if let ParsedExpr::Subscript { array, index, .. } = expr {
                    Expr::Subscript {
                        ast: expr,
                        array: Box::new(item.compile_def_expr(def, array)),
                        index: Box::new(Expr::IndexRef {
                            ast: index,
                            range: range.clone(),
                        }),
                    }
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScalarTypeExpr<'ast> {
    pub ast: &'ast ParsedScalarTypeExpr,
    pub ty: ScalarType,
}
