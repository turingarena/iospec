use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;
use crate::hir_env::*;

fn hir_block(ast: ABlock, env: &Env) -> HBlock {
    let mut env = env.clone();
    let mut stmts = Vec::new();

    for stmt in ast.stmts {
        let stmt = hir_stmt(stmt, &env);
        env.refs.extend(stmt.var_decls.iter().cloned());
        stmts.push(HN::new(stmt));
    }

    HBlock {
        fun_decls: HN::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.fun_decls.iter())
                .cloned()
                .collect(),
        ),
        var_decls: HN::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.var_decls.iter())
                .cloned()
                .collect(),
        ),
        defs: HN::new(
            stmts
                .iter()
                .flat_map(|stmt| stmt.defs.iter())
                .cloned()
                .collect(),
        ),
        stmts: HN::new(stmts),
    }
}

fn hir_stmt(ast: AStmt, env: &Env) -> HStmt {
    match ast {
        AStmt::Read { inst, args, semi } => {
            let mut arg_commas = Vec::new();

            let args: Vec<HN<HDef>> = args
                .into_pairs()
                .map(|a| match a {
                    syn::punctuated::Pair::Punctuated(a, comma) => {
                        arg_commas.push(comma);
                        hir_def(a, env)
                    }
                    syn::punctuated::Pair::End(a) => hir_def(a, env),
                })
                .map(HN::new)
                .collect();

            HStmt {
                fun_decls: HN::new(Vec::new()),
                var_decls: HN::new(args.iter().map(hir_def_decl).map(HN::new).collect()),
                defs: HN::new(args.iter().map(|def| def.expr.clone()).collect()),
                kind: HStmtKind::Read {
                    inst,
                    args,
                    arg_commas,
                    semi,
                },
            }
        }
        AStmt::Write { inst, args, semi } => {
            let mut arg_commas = Vec::new();

            HStmt {
                fun_decls: HN::new(Vec::new()),
                var_decls: HN::new(Vec::new()),
                defs: HN::new(Vec::new()),
                kind: HStmtKind::Write {
                    inst,
                    args: HN::new(
                        args.into_pairs()
                            .map(|a| match a {
                                syn::punctuated::Pair::Punctuated(a, comma) => {
                                    arg_commas.push(comma);
                                    hir_val_expr(a, env)
                                }
                                syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                            })
                            .map(HN::new)
                            .collect(),
                    ),
                    arg_commas,
                    semi,
                },
            }
        }
        AStmt::Call {
            inst,
            name,
            args_paren,
            args,
            ret,
            semi,
        } => {
            let mut arg_commas = Vec::new();

            let (ret_rarrow, ret) = match ret {
                Some((a, r)) => (Some(a), Some(HN::new(hir_def(r, env)))),
                None => (None, None),
            };

            let args: Vec<HN<HValExpr>> = args
                .into_pairs()
                .map(|a| match a {
                    syn::punctuated::Pair::Punctuated(a, comma) => {
                        arg_commas.push(comma);
                        hir_val_expr(a, env)
                    }
                    syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                })
                .map(HN::new)
                .collect();

            let ret_var = ret.as_ref().map(hir_def_decl).map(HN::new);
            let ret_def = ret.as_ref().map(|r| r.expr.clone());

            let fun = HFun {
                name: HN::new(hir_ident(name, env)),
                params: HN::new(
                    args.iter()
                        .map(|a| HParam {
                            name: match &a.kind {
                                HValExprKind::Ref { ident, .. } => ident.clone(),
                                _ => todo!("recover from non-simple-var arg"),
                            },
                            ty: a.ty.clone(),
                        })
                        .map(HN::new)
                        .collect(),
                ),
                ret,
            };
            let fun = HN::new(fun);

            HStmt {
                fun_decls: HN::new(vec![fun.clone()]),
                var_decls: HN::new(ret_var.iter().cloned().collect()),
                defs: HN::new(ret_def.iter().cloned().collect()),
                kind: HStmtKind::Call {
                    inst,
                    fun,
                    args: HN::new(args),
                    args_paren,
                    arg_commas,
                    ret_rarrow,
                    semi,
                },
            }
        }
        AStmt::For {
            for_token,
            index_name,
            upto,
            bound,
            body_brace,
            body,
        } => {
            let index_name = HN::new(hir_ident(index_name, env));
            let range = HN::new(HRange {
                bound: HN::new(hir_val_expr(bound, env)),
                upto,
                index_name: index_name.clone(),
            });

            let body = hir_block(
                body,
                &Env {
                    refs: vec![HN::new(HVarDecl {
                        ident: index_name,
                        kind: HDeclKind::Index {
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
                fun_decls: body.fun_decls.clone(),
                var_decls: body.var_decls.clone(),
                defs: HN::new(
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
                    for_token,
                    range,
                    body_brace,
                    body: HN::new(body),
                },
            }
        }
    }
}

fn hir_def(ast: ADef, env: &Env) -> HDef {
    let ADef { expr, colon, ty } = ast;
    let atom_ty = HN::new(hir_atom_ty(ty, env));

    let expr = hir_def_expr(
        expr,
        env,
        &HN::new(HExprTy::Atom {
            atom: atom_ty.clone(),
        }),
        &env.cons_path.clone(),
    );

    HDef {
        colon,
        atom_ty,
        ident: expr.ident.clone(),
        var_ty: expr.var_ty.clone(),
        expr: HN::new(expr),
    }
}

fn hir_def_expr(ast: AExpr, env: &Env, ty: &HN<HExprTy>, cons_path: &ConsPath) -> HDefExpr {
    match ast {
        AExpr::Ref { ident } => {
            let ident = HN::new(hir_ident(ident, env));
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
                let array_ty = HN::new(HExprTy::Array {
                    item: ty.clone(),
                    range: range.clone(),
                });

                let array = hir_def_expr(*array, env, &array_ty, parent);

                HDefExpr {
                    ident: array.ident.clone(),
                    var_ty: array.var_ty.clone(),
                    expr_ty: ty.clone(),
                    kind: HDefExprKind::Subscript {
                        array: HN::new(array),
                        index: HN::new(hir_val_expr(*index, env)),
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
            let ident = hir_ident(ident, env);
            let target = env.resolve(&ident);

            let ty = match &target
                .as_ref()
                .unwrap_or_else(|| todo!("recover from undefined var"))
                .kind
            {
                HDeclKind::Var { def } => def.var_ty.clone(),
                HDeclKind::Index { range } => HN::new(HExprTy::Index {
                    range: range.clone(),
                }),
            };

            HValExpr {
                ty,
                kind: HValExprKind::Ref {
                    target,
                    ident: HN::new(ident),
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
                    array: HN::new(array),
                    index: HN::new(index),
                    bracket,
                },
            }
        }
    }
}

fn hir_atom_ty(ast: ATy, env: &Env) -> HAtomTy {
    HAtomTy {
        ident: HN::new(hir_ident(ast.ident, env)),
    }
}

fn hir_ident(ast: AIdent, env: &Env) -> HIdent {
    let AIdent { token } = ast;
    HIdent { token }
}

fn hir_def_decl(def: &HN<HDef>) -> HVarDecl {
    HVarDecl {
        ident: def.ident.clone(),
        kind: HDeclKind::Var { def: def.clone() },
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
        fun_decls: main.fun_decls.clone(),
        main: HN::new(main),
    }
}
