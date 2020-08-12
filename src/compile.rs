use crate::parse::*;

#[derive(Debug, Clone)]
pub enum Scope<'ast> {
    Root(ScopeRoot),
    Decl(ScopeDecl<'ast>),
    For(ScopeFor<'ast>),
    Loop(ScopeLoop<'ast>),
}

#[derive(Debug, Clone)]
pub struct ScopeRoot;

#[derive(Debug, Clone)]
pub struct ScopeDecl<'ast> {
    pub decl: CompiledDecl<'ast>,
    pub parent: Box<Scope<'ast>>,
}

#[derive(Debug, Clone)]
pub struct ScopeFor<'ast> {
    //    pub for_stmt: &'ast StmtFor,
    pub parent: Box<Scope<'ast>>,
}

#[derive(Debug, Clone)]
pub struct ScopeLoop<'ast> {
    //    pub loop_stmt: &'ast StmtLoop,
    pub parent: Box<Scope<'ast>>,
}

#[derive(Debug, Clone)]
enum NameResolution<'ast> {
    Decl(CompiledDecl<'ast>),
}

impl<'ast> Scope<'ast> {
    fn resolve(self: &Self, name: &str) -> Option<NameResolution<'ast>> {
        match self {
            Scope::Decl(ScopeDecl { decl, parent }) => {
                if decl.name == name {
                    Some(NameResolution::Decl(decl.clone()))
                } else {
                    parent.resolve(name)
                }
            }
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

#[derive(Debug, Copy, Clone)]
pub enum ScalarType {
    I32,
    N32,
    I64,
    N64,
}

#[derive(Debug, Clone)]
pub struct CompiledScalarTypeExpr<'ast> {
    pub ast: &'ast ParsedScalarTypeExpr,
    pub ty: ScalarType,
}

impl<'ast> Compile<'ast, ParsedScalarTypeExpr> for CompiledScalarTypeExpr<'ast> {
    fn compile(ast: &'ast ParsedScalarTypeExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
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
    pub ty: CompiledScalarTypeExpr<'ast>,
}

impl CompiledDecl<'_> {
    pub fn expr(self: &Self) -> CompiledExpr {
        CompiledExpr::Var(CompiledExprVar {
            ast: match &self.ast.expr {
                ParsedExpr::Var(ast) => ast,
                _ => unreachable!(),
            },
            decl: self.clone(),
        })
    }
}

impl<'ast> Compile<'ast, ParsedDecl> for CompiledDecl<'ast> {
    fn compile(ast: &'ast ParsedDecl, scope: &Scope<'ast>) -> CompileResult<Self> {
        let name = match &ast.expr {
            ParsedExpr::Var(ParsedExprVar {
                ident: ParsedIdent { sym },
            }) => sym,
            _ => Err("unsupported expression in declaration")?,
        };

        Ok(Self {
            ast,
            name,
            ty: compile(&ast.ty, &scope)?,
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
            CompiledExpr::Var(expr) => expr.decl.ty.ty,
            CompiledExpr::Index(expr) => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompiledExprVar<'ast> {
    pub ast: &'ast ParsedExprVar,
    pub decl: CompiledDecl<'ast>,
}

#[derive(Debug, Clone)]
pub struct CompiledExprIndex<'ast> {
    pub ast: &'ast ParsedExprIndex,
    pub array: Box<CompiledExpr<'ast>>,
    pub index: Box<CompiledExpr<'ast>>,
}

impl<'ast> Compile<'ast, ParsedExpr> for CompiledExpr<'ast> {
    fn compile(ast: &'ast ParsedExpr, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedExpr::Var(ast) => CompiledExpr::Var(CompiledExprVar {
                ast,
                decl: match scope.resolve(&ast.ident.sym) {
                    Some(NameResolution::Decl(decl)) => decl,
                    _ => panic!("undefined variable"),
                },
            }),
            ParsedExpr::Index(ast) => CompiledExpr::Index(CompiledExprIndex {
                ast,
                array: Box::new(compile(ast.array.as_ref(), scope)?),
                index: Box::new(compile(ast.index.as_ref(), scope)?),
            }),
        })
    }
}

#[derive(Debug, Clone)]
pub enum CompiledStmt<'ast> {
    Read(CompiledStmtRead<'ast>),
    Write(CompiledStmtWrite<'ast>),
    Call(CompiledStmtCall<'ast>),
}

#[derive(Debug, Clone)]
pub struct CompiledStmtRead<'ast> {
    pub ast: &'ast ParsedStmtRead,
    pub args: Vec<CompiledDecl<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtWrite<'ast> {
    pub ast: &'ast ParsedStmtWrite,
    pub args: Vec<CompiledExpr<'ast>>,
}

#[derive(Debug, Clone)]
pub struct CompiledStmtCall<'ast> {
    pub ast: &'ast ParsedStmtCall,
    pub name: &'ast str,
    pub args: Vec<CompiledExpr<'ast>>,
    pub return_value: Option<CompiledDecl<'ast>>,
}

impl<'ast> CompiledStmt<'ast> {
    fn extend_scope(self: &Self, scope: Scope<'ast>) -> Scope<'ast> {
        match self {
            CompiledStmt::Read(CompiledStmtRead { args, .. }) => {
                let mut current = scope;
                for decl in args {
                    current = Scope::Decl(ScopeDecl {
                        decl: decl.clone(),
                        parent: Box::new(current.clone()),
                    })
                }
                current
            }
            CompiledStmt::Call(CompiledStmtCall {
                return_value: Some(return_value),
                ..
            }) => Scope::Decl(ScopeDecl {
                decl: return_value.clone(),
                parent: Box::new(scope.clone()),
            }),
            _ => scope,
        }
    }
}

impl<'ast> Compile<'ast, ParsedStmt> for CompiledStmt<'ast> {
    fn compile(ast: &'ast ParsedStmt, scope: &Scope<'ast>) -> CompileResult<Self> {
        Ok(match ast {
            ParsedStmt::Read(ast) => CompiledStmt::Read(CompiledStmtRead {
                ast,
                args: ast
                    .args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Write(ast) => CompiledStmt::Write(CompiledStmtWrite {
                ast,
                args: ast
                    .args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
            }),
            ParsedStmt::Call(ast) => CompiledStmt::Call(CompiledStmtCall {
                ast,
                name: &ast.name.sym,
                args: ast
                    .args
                    .iter()
                    .map(|i| compile(i, scope))
                    .collect::<CompileResult<Vec<_>>>()?,
                return_value: match &ast.return_value {
                    Some((_, decl)) => Some(compile(decl, scope)?),
                    None => None,
                },
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
        main: compile(&ast.main, &Scope::Root(ScopeRoot))?,
    })
}
