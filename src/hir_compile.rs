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

trait HirCompileFrom<T> {
    fn compile(ast: T, env: &Env) -> Self;
}

trait HirCompileInto<T> {
    fn compile(self: Self, env: &Env) -> Rc<T>;
}

impl<T, U> HirCompileInto<T> for U
where T: HirCompileFrom<U> {
    fn compile(self: Self, env: &Env) -> Rc<T> {
        Rc::new(T::compile(self, env))
    }
}

impl HirCompileFrom<ABlock> for HStmt {
    fn compile(ast: ABlock, env: &Env) -> Self {
        let mut env = env.clone();
        let mut stmts = Vec::new();

        for stmt in ast.stmts {
            let stmt = hir_stmt(stmt, &env);
            env.refs.extend(stmt.vars().into_iter());
            stmts.push(Rc::new(stmt));
        }

        HStmt::Block { stmts }
    }
}

fn hir_stmt(ast: AStmt, env: &Env) -> HStmt {
    match ast {
        AStmt::Read { kw, args, semi } => {
            let (args, arg_commas) = unzip_punctuated(args);

            HStmt::Read {
                kw,
                args: args
                    .into_iter()
                    .map(|a| hir_def(a, env))
                    .map(Rc::new)
                    .collect(),
                arg_commas,
                semi,
            }
        }
        AStmt::Write { kw, args, semi } => {
            let (args, arg_commas) = unzip_punctuated(args);

            HStmt::Write {
                kw,
                args: args
                    .into_iter()
                    .map(|a| hir_val_expr(a, env))
                    .map(Rc::new)
                    .collect(),
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
                Some((a, r)) => (Some(a), Some(Rc::new(hir_def(r, env)))),
                None => (None, None),
            };

            HStmt::Call {
                kw,
                fun: Rc::new(HFun {
                    name: Rc::new(hir_ident(name)),
                    args: args
                        .into_iter()
                        .map(|a| hir_val_expr(a, env))
                        .map(Rc::new)
                        .collect(),
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
                index: Rc::new(hir_ident(index)),
                upto,
                bound: Rc::new(hir_val_expr(bound, env)),
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

fn hir_def(ast: ADef, env: &Env) -> HDef {
    let ADef { expr, colon, ty } = ast;
    let ty = Rc::new(hir_atom_ty(ty));

    HDef {
        colon,
        expr: Rc::new(hir_def_expr(
            expr,
            env,
            ty.clone(),
            Rc::new(HDefExprCtx::Atom),
            env.loc.clone(),
        )),
        ty,
        loc: env.loc.clone(),
    }
}

fn hir_def_expr(
    ast: AExpr,
    env: &Env,
    atom_ty: Rc<HAtomTy>,
    ctx: Rc<HDefExprCtx>,
    loc: Rc<HDefLoc>,
) -> HDefExpr {
    HDefExpr {
        atom_ty: atom_ty.clone(),
        ctx: ctx.clone(),
        loc: loc.clone(),
        kind: match ast {
            AExpr::Ref { ident } => HDefExprKind::Var {
                ident: Rc::new(hir_ident(ident)),
            },
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => {
                let index = Rc::new(hir_val_expr(*index, env));

                HDefExprKind::Subscript {
                    array: Rc::new(hir_def_expr(
                        *array,
                        env,
                        atom_ty,
                        Rc::new(HDefExprCtx::Subscript {
                            item: ctx,
                            index: index.clone(),
                        }),
                        loc,
                    )),
                    index,
                    bracket,
                }
            }
        },
    }
}

fn hir_val_expr(ast: AExpr, env: &Env) -> HValExpr {
    match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident);
            let var = env
                .resolve(&ident)
                .unwrap_or_else(|| todo!("recover from undefined var"));

            HValExpr::Var {
                var,
                ident: Rc::new(ident),
            }
        }
        AExpr::Subscript {
            array,
            bracket,
            index,
        } => HValExpr::Subscript {
            array: Rc::new(hir_val_expr(*array, env)),
            index: Rc::new(hir_val_expr(*index, env)),
            bracket,
        },
    }
}

fn hir_atom_ty(ast: ATy) -> HAtomTy {
    HAtomTy {
        ident: Rc::new(hir_ident(ast.ident)),
    }
}

fn hir_ident(ast: AIdent) -> HIdent {
    let AIdent { token } = ast;
    HIdent { token }
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
