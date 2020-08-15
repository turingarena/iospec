use crate::ast::*;

#[derive(Debug, Clone)]
pub struct Block<'ast> {
    pub ast: &'ast AstBlock,
    pub stmts: Vec<Stmt<'ast>>,
}

impl<'ast> Block<'ast> {
    pub fn defs(self: &Self) -> Vec<Def<'ast>> {
        self.stmts.iter().flat_map(Stmt::defs).collect()
    }
}

#[derive(Debug, Clone)]
pub struct Def<'ast> {
    pub ast: &'ast AstDef,
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


#[derive(Debug, Clone)]
pub enum Expr<'ast> {
    VarRef {
        ast: &'ast AstExpr,
        def: Def<'ast>,
    },
    IndexRef {
        ast: &'ast AstExpr,
        range: Box<Range<'ast>>,
    },
    Subscript {
        ast: &'ast AstExpr,
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

#[derive(Debug, Clone)]
pub struct Range<'ast> {
    pub stmt_ast: &'ast AstStmt,
    pub index_name: &'ast str,
    pub bound: Expr<'ast>,
}


#[derive(Debug, Clone)]
pub struct Spec<'ast> {
    pub ast: &'ast AstSpec,
    pub main: Block<'ast>,
}


#[derive(Debug, Clone)]
pub enum Stmt<'ast> {
    Read {
        ast: &'ast AstStmt,
        args: Vec<Def<'ast>>,
    },
    Write {
        ast: &'ast AstStmt,
        args: Vec<Expr<'ast>>,
    },
    Call {
        ast: &'ast AstStmt,
        name: &'ast str,
        args: Vec<Expr<'ast>>,
        return_value: Option<Def<'ast>>,
    },
    For {
        ast: &'ast AstStmt,
        range: Range<'ast>,
        body: Block<'ast>,
    },
}

impl<'ast> Stmt<'ast> {
    pub fn defs(self: &Self) -> Vec<Def<'ast>> {
        match self {
            Stmt::Read { args, .. } => args.to_vec(),
            Stmt::Call {
                return_value: Some(return_value),
                ..
            } => vec![return_value.clone()],
            // TODO: make into decls
            Stmt::For { body, range, .. } => body.defs(),
            _ => vec![],
        }
    }
}

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

    pub fn compile_def_expr(self: &Self, def: Def<'ast>, expr: &'ast AstExpr) -> Expr {
        match self {
            VariableType::Scalar { .. } => Expr::VarRef { ast: expr, def },
            VariableType::Array { item, range } => {
                if let AstExpr::Subscript { array, index, .. } = expr {
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
    pub ast: &'ast AstScalarTypeExpr,
    pub ty: ScalarType,
}
