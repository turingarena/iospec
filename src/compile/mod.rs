use crate::parse::*;

mod scope;

use scope::*;

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

#[derive(Debug, Copy, Clone)]
pub enum ScalarType {
    I32,
    N32,
    I64,
    N64,
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Scalar(ScalarVariableType),
//    Array(ArrayVariableType),
}

#[derive(Debug, Clone)]
pub struct ScalarVariableType {
    scalar_type: ScalarType,
}

#[derive(Debug, Clone)]
pub struct ArrayVariableType {
    array_type: Box<VariableType>,
//    index_decl:
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

#[derive(Debug, Clone)]
pub struct CompiledDecl<'ast> {
    pub ast: &'ast ParsedDecl,
    pub name: &'ast str,
    // TODO: support array types
    pub scalar_type_expr: CompiledScalarTypeExpr<'ast>,
}

impl CompiledDecl<'_> {
    pub fn expr(self: &Self) -> CompiledExpr {
        CompiledExpr::Var(CompiledExprVar {
            ast: &self.ast.expr,
            decl: self.clone(),
        })
    }
}

impl<'ast> Compile<'ast, ParsedDecl> for CompiledDecl<'ast> {
    fn compile(ast: &'ast ParsedDecl, scope: &Scope<'ast>) -> CompileResult<Self> {
        let name = match &ast.expr {
            ParsedExpr::Var {
                ident: ParsedIdent { sym },
            } => sym,
            _ => Err("unsupported expression in declaration")?,
        };

        Ok(Self {
            ast,
            name,
            scalar_type_expr: compile(&ast.ty, &scope)?,
        })
    }
}

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

#[derive(Debug, Clone)]
pub enum CompiledStmt<'ast> {
    Read(CompiledStmtRead<'ast>),
    Write(CompiledStmtWrite<'ast>),
    Call(CompiledStmtCall<'ast>),
    For(CompiledStmtFor<'ast>),
}

#[derive(Debug, Clone)]
pub struct CompiledStmtRead<'ast> {
    pub ast: &'ast ParsedStmt,
    pub args: Vec<CompiledDecl<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtWrite<'ast> {
    pub ast: &'ast ParsedStmt,
    pub args: Vec<CompiledExpr<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtCall<'ast> {
    pub ast: &'ast ParsedStmt,
    pub name: &'ast str,
    pub args: Vec<CompiledExpr<'ast>>,
    pub return_value: Option<CompiledDecl<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtFor<'ast> {
    pub ast: &'ast ParsedStmt,
    pub index_name: &'ast str,
    pub range: CompiledExpr<'ast>,
    pub body: CompiledBlock<'ast>,
}

impl<'ast> CompiledStmt<'ast> {
    fn extend_scope(self: &Self, scope: Scope<'ast>) -> Scope<'ast> {
        match self {
            CompiledStmt::Read(CompiledStmtRead { args, .. }) => {
                let mut current = scope;
                for decl in args {
                    current = Scope::Decl {
                        decl: decl.clone(),
                        parent: Box::new(current.clone()),
                    }
                }
                current
            }
            CompiledStmt::Call(CompiledStmtCall {
                return_value: Some(return_value),
                ..
            }) => Scope::Decl {
                decl: return_value.clone(),
                parent: Box::new(scope.clone()),
            },
            _ => scope,
        }
    }
}

impl<'ast> Compile<'ast, ParsedStmt> for CompiledStmt<'ast> {
    fn compile(ast: &'ast ParsedStmt, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedStmt::Read { args, .. } => CompiledStmt::Read(CompiledStmtRead {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Write { args, .. } => CompiledStmt::Write(CompiledStmtWrite {
                ast,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Call { name, args, return_value, .. } => CompiledStmt::Call(CompiledStmtCall {
                ast,
                name: &name.sym,
                args: args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
                return_value: match &return_value {
                    Some((_, decl)) => Some(compile(decl, scope)?),
                    None => None,
                },
            }),
            ParsedStmt::For { index_name, range, body, .. } => CompiledStmt::For(CompiledStmtFor {
                ast,
                index_name: &index_name.sym,
                range: compile(range, &scope)?,
                // TODO: change the scope to include the index
                body: compile(body, &scope)?,
            }),
        })
    }
}

#[derive(Debug, Clone)]
pub struct CompiledBlock<'ast> {
    pub ast: &'ast ParsedBlock,
    pub stmts: Vec<CompiledStmt<'ast>>,
}

impl<'ast> Compile<'ast, ParsedBlock> for CompiledBlock<'ast> {
    fn compile(ast: &'ast ParsedBlock, scope: &Scope<'ast>) -> CompileResult<Self> {
        let mut stmts = vec![];
        let mut scope = scope.clone();

        for ast in ast.stmts.iter() {
            let stmt: CompiledStmt = compile(ast, &scope)?;
            scope = stmt.extend_scope(scope);
            stmts.push(stmt);
        }

        Ok(CompiledBlock { ast, stmts })
    }
}

#[derive(Debug, Clone)]
pub struct CompiledSpec<'ast> {
    pub ast: &'ast ParsedSpec,
    pub main: CompiledBlock<'ast>,
}

pub fn compile_spec(ast: &ParsedSpec) -> CompileResult<CompiledSpec> {
    Ok(CompiledSpec {
        ast,
        main: compile(&ast.main, &Scope::Root)?,
    })
}
