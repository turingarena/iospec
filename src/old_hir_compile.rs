use crate::old_hir::*;
use crate::ast::*;

#[derive(Debug, Clone)]
pub enum Scope<'ast> {
    Root,
    Def {
        def: Def<'ast>,
        parent: Box<Scope<'ast>>,
    },
    For {
        range: Range<'ast>,
        parent: Box<Scope<'ast>>,
    },
    Loop {
        // loop_stmt: &'ast StmtLoop,
        parent: Box<Scope<'ast>>,
    },
}

#[derive(Debug, Clone)]
pub enum NameResolution<'ast> {
    Def(Def<'ast>),
    Index(Range<'ast>),
}

impl<'ast> Scope<'ast> {
    pub fn resolve(self: &Self, name: &str) -> Option<NameResolution<'ast>> {
        match self {
            Scope::Def { def, parent } => {
                if def.name == name {
                    Some(NameResolution::Def(def.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            Scope::For { range, parent } => {
                if range.index_name == name {
                    Some(NameResolution::Index(range.clone()))
                } else {
                    parent.resolve(name)
                }
            }
            _ => None,
        }
    }

    pub fn find_innermost_for(self: &Self) -> Option<(Range<'ast>, Scope<'ast>)> {
        match self {
            Scope::For { range, parent } => Some((range.clone(), parent.as_ref().clone())),
            Scope::Def { parent, .. } => parent.find_innermost_for(),
            _ => None,
        }
    }
}

type CompileResult<T> = Result<T, String>;

trait Compile<'ast, T>
    where
        Self: std::marker::Sized,
{
    fn compile(ast: &'ast T, scope: &Scope<'ast>) -> CompileResult<Self>;
}

fn compile<'ast, T, U>(ast: &'ast T, scope: &Scope<'ast>) -> CompileResult<U>
    where
        U: Compile<'ast, T>,
{
    U::compile(ast, scope)
}

impl<'ast> Compile<'ast, AstBlock> for Block<'ast> {
    fn compile(ast: &'ast AstBlock, scope: &Scope<'ast>) -> CompileResult<Self> {
        let mut stmts = vec![];
        let mut scope = scope.clone();

        for ast in ast.stmts.iter() {
            let stmt: Stmt = compile(ast, &scope)?;

            for def in stmt.defs() {
                scope = Scope::Def {
                    def,
                    parent: Box::new(scope),
                };
            }
            stmts.push(stmt);
        }

        Ok(Block { ast, stmts })
    }
}


impl<'ast> Compile<'ast, AstDef> for Def<'ast> {
    fn compile(ast: &'ast AstDef, scope: &Scope<'ast>) -> CompileResult<Self> {
        let value_type_expr: ScalarTypeExpr = compile(&ast.ty, &scope)?;

        let mut variable_type = VariableType::Scalar {
            expr: value_type_expr.clone(),
        };

        let mut expr = &ast.expr;
        let mut scope = scope.clone();

        while let AstExpr::Subscript { array, index, .. } = expr {
            let index_name = if let AstExpr::Ref { ident } = index.as_ref() {
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
            AstExpr::Ref {
                ident: AstIdent { sym },
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


impl<'ast> Compile<'ast, AstExpr> for Expr<'ast> {
    fn compile(ast: &'ast AstExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            AstExpr::Ref { ident } => match scope.resolve(&ident.sym) {
                Some(NameResolution::Def(def)) => Expr::VarRef { ast, def },
                Some(NameResolution::Index(range)) => Expr::IndexRef {
                    ast,
                    range: range.into(),
                },
                _ => panic!("undefined variable"),
            },
            AstExpr::Subscript { array, index, .. } => Expr::Subscript {
                ast,
                array: Box::new(compile(array.as_ref(), scope)?),
                index: Box::new(compile(index.as_ref(), scope)?),
            },
        })
    }
}


impl<'ast> Compile<'ast, AstStmt> for Stmt<'ast> {
    fn compile(ast: &'ast AstStmt, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            AstStmt::Read { args, .. } => Stmt::Read {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            },
            AstStmt::Write { args, .. } => Stmt::Write {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            },
            AstStmt::Call {
                name,
                args,
                return_value,
                ..
            } => Stmt::Call {
                ast,
                name: &name.sym,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
                return_value: match &return_value {
                    Some((_, def)) => Some(compile(def, scope)?),
                    None => None,
                },
            },
            AstStmt::For {
                index_name,
                bound,
                body,
                ..
            } => {
                let range = Range {
                    stmt_ast: ast,
                    index_name: &index_name.sym,
                    bound: compile(bound, &scope)?,
                };
                Stmt::For {
                    ast,
                    range: range.clone(),
                    body: compile(
                        body,
                        &Scope::For {
                            parent: Box::new(scope.clone()),
                            range,
                        },
                    )?,
                }
            }
        })
    }
}


impl<'ast> Compile<'ast, AstScalarTypeExpr> for ScalarTypeExpr<'ast> {
    fn compile(ast: &'ast AstScalarTypeExpr, _scope: &Scope<'ast>) -> CompileResult<Self> {
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

pub fn compile_spec(ast: &AstSpec) -> CompileResult<Spec> {
    Ok(Spec {
        ast,
        main: compile(&ast.main, &Scope::Root)?,
    })
}
