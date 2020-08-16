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

fn hir_block(ast: ABlock, env: &Env) -> HBlock {
    let mut env = env.clone();
    let mut stmts = Vec::new();

    for stmt in ast.stmts {
        let stmt = hir_stmt(stmt, &env);
        env.refs.extend(stmt.vars().into_iter());
        stmts.push(Rc::new(stmt));
    }

    HBlock {
        stmts: Rc::new(stmts),
    }
}

fn hir_stmt(ast: AStmt, env: &Env) -> HStmt {
    match ast {
        AStmt::Read { kw, args, semi } => {
            let mut arg_commas = Vec::new();

            let args: Vec<Rc<HDef>> = args
                .into_pairs()
                .map(|a| match a {
                    syn::punctuated::Pair::Punctuated(a, comma) => {
                        arg_commas.push(comma);
                        hir_def(a, env)
                    }
                    syn::punctuated::Pair::End(a) => hir_def(a, env),
                })
                .map(Rc::new)
                .collect();

            HStmt {
                kind: HStmtKind::Read {
                    kw,
                    args,
                    arg_commas,
                    semi,
                },
            }
        }
        AStmt::Write { kw, args, semi } => {
            let mut arg_commas = Vec::new();

            HStmt {
                kind: HStmtKind::Write {
                    kw,
                    args: Rc::new(
                        args.into_pairs()
                            .map(|a| match a {
                                syn::punctuated::Pair::Punctuated(a, comma) => {
                                    arg_commas.push(comma);
                                    hir_val_expr(a, env)
                                }
                                syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                            })
                            .map(Rc::new)
                            .collect(),
                    ),
                    arg_commas,
                    semi,
                },
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
            let mut arg_commas = Vec::new();

            let (ret_rarrow, ret) = match ret {
                Some((a, r)) => (Some(a), Some(Rc::new(hir_def(r, env)))),
                None => (None, None),
            };

            let args: Vec<Rc<HValExpr>> = args
                .into_pairs()
                .map(|a| match a {
                    syn::punctuated::Pair::Punctuated(a, comma) => {
                        arg_commas.push(comma);
                        hir_val_expr(a, env)
                    }
                    syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                })
                .map(Rc::new)
                .collect();

            let fun = HFun {
                name: Rc::new(hir_ident(name)),
                args: Rc::new(args),
                ret,
                args_paren,
                arg_commas,
                ret_rarrow,
            };
            let fun = Rc::new(fun);

            HStmt {
                kind: HStmtKind::Call { kw, fun, semi },
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
            let index = Rc::new(hir_ident(index));
            let range = Rc::new(HRange {
                bound: Rc::new(hir_val_expr(bound, env)),
                upto,
                index: index.clone(),
            });

            let body = hir_block(
                body,
                &Env {
                    refs: vec![Rc::new(hir_index_var(&range))],
                    outer: Some(Box::new((*env).clone())),
                    loc: Rc::new(HDefLoc::For {
                        range: range.clone(),
                        parent: env.loc.clone(),
                    }),
                },
            );

            HStmt {
                kind: HStmtKind::For {
                    kw,
                    range,
                    body_brace,
                    body: Rc::new(body),
                },
            }
        }
    }
}

fn hir_def(ast: ADef, env: &Env) -> HDef {
    let ADef { expr, colon, ty } = ast;
    let ty = Rc::new(hir_atom_ty(ty));

    let expr = hir_def_expr(
        expr,
        env,
        ty.clone(),
        Rc::new(HDefExprCtx::Atom),
        env.loc.clone(),
    );

    HDef {
        colon,
        ty,
        expr: Rc::new(expr),
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

                let array_ctx = HDefExprCtx::Subscript {
                    item: ctx,
                    index: index.clone(),
                };
                let array = Rc::new(hir_def_expr(*array, env, atom_ty, Rc::new(array_ctx), loc));

                HDefExprKind::Subscript {
                    array,
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

            HValExpr {
                kind: HValExprKind::Var {
                    var,
                    ident: Rc::new(ident),
                },
            }
        }
        AExpr::Subscript {
            array,
            bracket,
            index,
        } => {
            let array = hir_val_expr(*array, env);
            let index = hir_val_expr(*index, env);

            HValExpr {
                kind: HValExprKind::Subscript {
                    array: Rc::new(array),
                    index: Rc::new(index),
                    bracket,
                },
            }
        }
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
    let main = hir_block(
        ast.main,
        &Env {
            refs: Vec::new(),
            outer: None,
            loc: Rc::new(HDefLoc::Main),
        },
    );

    HSpec {
        main: Rc::new(main),
    }
}
