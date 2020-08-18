//! Transform an AST into HIR.
//!
//! Transformation is done by traversing the AST tree in post-order and generating HIR nodes,
//! while keeping and updating the environment (see `hir_env`) of each AST node
//! encountered in the traversal.
//! At the end the environment is discarded, and only the HIR (with its internal links) is kept.

use crate::ast::*;
use crate::hir::*;
use crate::hir_analyze::*;
use crate::hir_env::*;
use std::ops::Deref;

trait HirCompileFrom<T, E = Env> {
    fn compile(ast: T, env: &E) -> Self;
}

impl<T, U, E> HirCompileFrom<Box<T>, E> for U
    where
        U: HirCompileFrom<T, E>,
{
    fn compile(ast: Box<T>, env: &E) -> Self {
        U::compile(*ast, env)
    }
}

trait HirCompileInto<T, E> {
    fn compile(self: Self, env: &E) -> Rc<T>;
}

impl<U, T, E> HirCompileInto<U, E> for T
    where
        U: HirCompileFrom<T, E>,
{
    fn compile(self: Self, env: &E) -> Rc<U> {
        Rc::new(U::compile(self, env))
    }
}

impl HirCompileFrom<ABlock> for HStmt {
    fn compile(ast: ABlock, env: &Env) -> Self {
        let mut env = env.clone();
        let mut stmts = Vec::new();

        for stmt in ast.stmts {
            let stmt: Rc<HStmt> = stmt.compile(&env);
            env.refs.extend(stmt.vars().into_iter());
            stmts.push(stmt);
        }

        HStmt::Block { stmts }
    }
}

impl HirCompileFrom<AStmt> for HStmt {
    fn compile(ast: AStmt, env: &Env) -> Self {
        match ast {
            AStmt::Read { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmt::Read {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Write { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmt::Write {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Call {
                kw,
                name,
                args_paren,
                args,
                ret,
                semi,
            } => {
                let (args, arg_commas) = unzip_punctuated(args);
                let (ret_rarrow, ret) = match ret {
                    Some((a, r)) => (Some(a), Some(r.compile(env))),
                    None => (None, None),
                };

                HStmt::Call {
                    kw,
                    fun: Rc::new(HFun {
                        name: name.compile(&()),
                        args: args.into_iter().map(|a| a.compile(env)).collect(),
                        ret,
                        args_paren,
                        arg_commas,
                        ret_rarrow,
                    }),
                    semi,
                }
            }
            AStmt::For {
                kw,
                index,
                upto,
                bound,
                body_brace,
                body,
            } => {
                let range = Rc::new(HRange {
                    index: index.compile(&()),
                    upto,
                    bound: bound.compile(env),
                });

                HStmt::For {
                    kw,
                    body: body.compile(&Env {
                        refs: vec![Rc::new(hir_index_var(&range))],
                        outer: Some(Box::new((*env).clone())),
                        loc: Rc::new(HDefLoc::For {
                            range: range.clone(),
                            parent: env.loc.clone(),
                        }),
                    }),
                    range,
                    body_brace,
                }
            }
        }
    }
}

impl HirCompileFrom<ADef> for HDef {
    fn compile(ast: ADef, env: &Env) -> Self {
        let ADef { expr, colon, ty } = ast;
        let ty: Rc<HAtomTy> = ty.compile(&());

        HDef {
            colon,
            expr: expr.compile(&HDefEnv {
                env: env.clone(),
                atom_ty: ty.clone(),
                ctx: Rc::new(HDefExprCtx::Atom),
                loc: env.loc.clone(),
            }),
            ty,
            loc: env.loc.clone(),
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HDefExpr {
    fn compile(ast: AExpr, env: &HDefEnv) -> Self {
        HDefExpr {
            atom_ty: env.atom_ty.clone(),
            ctx: env.ctx.clone(),
            loc: env.loc.clone(),
            kind: match ast {
                AExpr::Ref { ident } => HDefExprKind::Var {
                    ident: ident.compile(&()),
                },
                AExpr::Subscript {
                    array,
                    bracket,
                    index,
                } => {
                    let index: Rc<HVal> = index.compile(&env.env);

                    HDefExprKind::Subscript {
                        array: array.compile(&HDefEnv {
                            env: env.env.clone(),
                            atom_ty: env.atom_ty.clone(),
                            ctx: Rc::new(HDefExprCtx::Subscript {
                                item: env.ctx.clone(),
                                index: index.clone(),
                            }),
                            loc: env.loc.clone(),
                        }),
                        index,
                        bracket,
                    }
                }
            },
        }
    }
}

impl HValExpr {
    fn ty(self: &Self) -> Rc<HExprTy> {
        match self {
            HValExpr::Var { var, .. } => var.ty.clone(),
            HValExpr::Subscript { array, .. } => match array.ty.deref() {
                // TODO: check index type as well
                HExprTy::Array { item, .. } => item.clone(),
                _ => todo!("recover from invalid array type"),
            },
        }
    }
}

impl HVal {
    fn name(self: &Self) -> Rc<HIdent> {
        match self.expr.deref() {
            HValExpr::Var { var, .. } => var.ident.clone(),
            _ => todo!("recover from invalid expr in call args"),
        }
    }
}

impl HirCompileFrom<AExpr> for HArg {
    fn compile(ast: AExpr, env: &Env) -> Self {
        let val: Rc<HVal> = ast.compile(env);

        HArg {
            name: val.name(),
            ty: val.ty.clone(),
            val,
        }
    }
}

impl HirCompileFrom<AExpr> for HVal {
    fn compile(ast: AExpr, env: &Env) -> Self {
        let expr: Rc<HValExpr> = ast.compile(env);

        HVal { ty: expr.ty(), expr }
    }
}

impl HirCompileFrom<AExpr> for HValExpr {
    fn compile(ast: AExpr, env: &Env) -> Self {
        match ast {
            AExpr::Ref { ident } => {
                let ident = ident.compile(&());
                let var = env
                    .resolve(&ident)
                    .unwrap_or_else(|| todo!("recover from undefined var"));

                HValExpr::Var { var, ident }
            }
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => HValExpr::Subscript {
                array: array.compile(env),
                index: index.compile(env),
                bracket,
            },
        }
    }
}

impl HirCompileFrom<ATy, ()> for HAtomTy {
    fn compile(ast: ATy, _: &()) -> Self {
        HAtomTy {
            ident: ast.ident.compile(&()),
        }
    }
}

impl HirCompileFrom<AIdent, ()> for HIdent {
    fn compile(ast: AIdent, _: &()) -> Self {
        let AIdent { token } = ast;
        HIdent { token }
    }
}

pub fn compile_hir(ast: ASpec) -> HSpec {
    HSpec {
        main: ast.main.compile(&Env {
            refs: Vec::new(),
            outer: None,
            loc: Rc::new(HDefLoc::Main),
        }),
    }
}

fn unzip_punctuated<T, U>(p: syn::punctuated::Punctuated<T, U>) -> (Vec<T>, Vec<U>) {
    let mut args = Vec::new();
    let mut puncts = Vec::new();
    for p in p.into_pairs() {
        let (a, p) = p.into_tuple();
        args.push(a);
        if let Some(p) = p {
            puncts.push(p)
        }
    }
    (args, puncts)
}
