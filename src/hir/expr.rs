use super::*;

#[derive(Debug, Clone)]
pub enum Expr<'ast> {
    VarRef {
        ast: &'ast ParsedExpr,
        def: Def<'ast>,
    },
    IndexRef {
        ast: &'ast ParsedExpr,
        range: Box<Range<'ast>>,
    },
    Subscript {
        ast: &'ast ParsedExpr,
        array: Box<Expr<'ast>>,
        index: Box<Expr<'ast>>,
    },
}

impl Expr<'_> {
    pub fn get_type(self: &Self) -> VariableType {
        self.try_get_type().unwrap()
    }

    pub fn try_get_type(self: &Self) -> Result<VariableType, Box<dyn std::error::Error>> {
        match self {
            Expr::VarRef { def, .. } => Ok(def.variable_type.clone()),
            Expr::IndexRef { range, .. } => Ok(VariableType::Index {
                range: range.clone(),
            }),
            Expr::Subscript { array, .. } => match array.try_get_type()? {
                VariableType::Array { item, .. } => Ok(item.as_ref().clone()),
                _ => Err("not an array, cannot index".into()),
            },
        }
    }
}
