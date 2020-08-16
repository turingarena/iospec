//! Transform an AST into HIR.
//!
//! Transformation is done by traversing the AST tree in post-order and generating HIR nodes,
//! while keeping and updating the environment (see `hir_env`) of each AST node
//! encountered in the traversal.
//! At the end the environment is discarded, and only the HIR (with its internal links) is kept.

use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;
use crate::hir_env::*;

fn hir_block(ast: ABlock, env: &Env) -> HBlock {
    let mut env = env.clone();
    let mut stmts = Vec::new();

    for stmt in ast.stmts {
        let stmt = hir_stmt(stmt, &env);
        env.refs.extend(stmt.vars.iter().cloned());
        stmts.push(Rc::new(stmt));
    }

    HBlock {
        funs: Rc::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.funs.iter())
                .cloned()
                .collect(),
        ),
        vars: Rc::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.vars.iter())
                .cloned()
                .collect(),
        ),
        defs: Rc::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.defs.iter())
                .cloned()
                .collect(),
        ),
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
                funs: Rc::new(Vec::new()),
                vars: Rc::new(args.iter().map(hir_def_decl).map(Rc::new).collect()),
                defs: Rc::new(args.iter().map(|def| def.expr.clone()).collect()),
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
                funs: Rc::new(Vec::new()),
                vars: Rc::new(Vec::new()),
                defs: Rc::new(Vec::new()),
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

            let ret_var = ret.as_ref().map(hir_def_decl).map(Rc::new);
            let ret_def = ret.as_ref().map(|r| r.expr.clone());

            let fun = HFun {
                name: Rc::new(hir_ident(name)),
                params: Rc::new(
                    args.iter()
                        .map(|a| HParam {
                            name: match &a.kind {
                                HValExprKind::Var { ident, .. } => ident.clone(),
                                _ => todo!("recover from non-simple-var arg"),
                            },
                            ty: a.ty.clone(),
                        })
                        .map(Rc::new)
                        .collect(),
                ),
                ret,
            };
            let fun = Rc::new(fun);

            HStmt {
                funs: Rc::new(vec![fun.clone()]),
                vars: Rc::new(ret_var.iter().cloned().collect()),
                defs: Rc::new(ret_def.iter().cloned().collect()),
                kind: HStmtKind::Call {
                    kw,
                    fun,
                    args: Rc::new(args),
                    args_paren,
                    arg_commas,
                    ret_rarrow,
                    semi,
                },
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
                    refs: vec![Rc::new(HVar {
                        ident: index,
                        kind: HVarKind::Index {
                            range: range.clone(),
                        },
                    })],
                    outer: Some(Box::new((*env).clone())),
                    cons_path: ConsPath::For {
                        range: range.clone(),
                        parent: Box::new(env.cons_path.clone()),
                    },
                },
            );

            HStmt {
                funs: body.funs.clone(),
                vars: body.vars.clone(),
                defs: Rc::new(
                    body.defs
                        .iter()
                        .flat_map(|expr| match &expr.kind {
                            // TODO: check index somewhere?
                            HDefExprKind::Subscript { array, .. } => Some(array.clone()),
                            _ => None,
                        })
                        .collect(),
                ),
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
    let atom_ty = Rc::new(hir_atom_ty(ty));

    let expr = hir_def_expr(
        expr,
        env,
        &Rc::new(HExprTy::Atom {
            atom: atom_ty.clone(),
        }),
        &env.cons_path.clone(),
    );

    HDef {
        colon,
        atom_ty,
        ident: expr.ident.clone(),
        var_ty: expr.var_ty.clone(),
        expr: Rc::new(expr),
    }
}

fn hir_def_expr(ast: AExpr, env: &Env, ty: &Rc<HExprTy>, cons_path: &ConsPath) -> HDefExpr {
    match ast {
        AExpr::Ref { ident } => {
            let ident = Rc::new(hir_ident(ident));
            HDefExpr {
                ident: ident.clone(),
                kind: HDefExprKind::Var { ident },
                expr_ty: ty.clone(),
                var_ty: ty.clone(),
            }
        }
        AExpr::Subscript {
            array,
            bracket,
            index,
        } => match cons_path {
            ConsPath::For { range, parent } => {
                let array_ty = Rc::new(HExprTy::Array {
                    item: ty.clone(),
                    range: range.clone(),
                });

                let array = hir_def_expr(*array, env, &array_ty, parent);

                HDefExpr {
                    ident: array.ident.clone(),
                    var_ty: array.var_ty.clone(),
                    expr_ty: ty.clone(),
                    kind: HDefExprKind::Subscript {
                        array: Rc::new(array),
                        index: Rc::new(hir_val_expr(*index, env)),
                        bracket,
                    },
                }
            }
            _ => todo!("recover"),
        },
    }
}

fn hir_val_expr(ast: AExpr, env: &Env) -> HValExpr {
    match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident);
            let var = env.resolve(&ident);

            let ty = match &var
                .as_ref()
                .unwrap_or_else(|| todo!("recover from undefined var"))
                .kind
            {
                HVarKind::Data { def } => def.var_ty.clone(),
                HVarKind::Index { range } => Rc::new(HExprTy::Index {
                    range: range.clone(),
                }),
            };

            HValExpr {
                ty,
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

            let ty = match array.ty.deref() {
                HExprTy::Array { item, .. } => item.clone(),
                _ => todo!("recover"),
            };

            HValExpr {
                ty,
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

fn hir_def_decl(def: &Rc<HDef>) -> HVar {
    HVar {
        ident: def.ident.clone(),
        kind: HVarKind::Data { def: def.clone() },
    }
}

pub fn compile_hir(ast: ASpec) -> HSpec {
    let main = hir_block(
        ast.main,
        &Env {
            refs: Vec::new(),
            outer: None,
            cons_path: ConsPath::Root,
        },
    );

    HSpec {
        funs: main.funs.clone(),
        main: Rc::new(main),
    }
}
