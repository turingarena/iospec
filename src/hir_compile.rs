use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;
use crate::hir_env::*;

fn hir_block(ast: ABlock, env: &mut Env) -> HN<HBlock> {
    let stmts: Vec<HN<HStmt>> = ast
        .stmts
        .into_iter()
        .map(|stmt| hir_stmt(stmt, env))
        .collect();
    HN::new(HBlock {
        defs: stmts
            .iter()
            .flat_map(|stmt| stmt.defs.iter())
            .cloned()
            .collect(),
        stmts,
    })
}

fn hir_def(ast: ADef, env: &mut Env) -> HN<HDef> {
    let ADef { expr, colon, ty } = ast;
    let atom_ty = hir_atom_ty(ty, env);

    let expr = hir_def_expr(
        expr,
        env,
        &HN::new(HExprTy::Atom {
            atom: atom_ty.clone(),
        }),
        &env.cons_path.clone(),
    );

    let def = HN::new(HDef {
        colon,
        atom_ty,
        var_ty: expr.expr_ty.clone(),
        expr,
    });

    env.refs.push(HN::new(HRef {
        ident: def.expr.ident.clone(),
        kind: HRefKind::Var { def: def.clone() },
    }));
    def
}

fn hir_val_expr(ast: AExpr, env: &mut Env) -> HN<HValExpr> {
    HN::new(match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident, env);
            let target = env.resolve(ident.clone());

            let ty = match &target.as_ref().unwrap_or_else(|| todo!("recover")).kind {
                HRefKind::Var { def } => def.var_ty.clone(),
                HRefKind::Index { range } => HN::new(HExprTy::Index {
                    range: range.clone(),
                }),
            };

            HValExpr {
                ty,
                kind: HValExprKind::Ref { target, ident },
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
                    array,
                    index,
                    bracket,
                },
            }
        }
    })
}

fn hir_def_expr(ast: AExpr, env: &mut Env, ty: &HN<HExprTy>, cons_path: &ConsPath) -> HN<HDefExpr> {
    HN::new(match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident, env);
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
                let ty = HN::new(HExprTy::Array {
                    item: ty.clone(),
                    range: range.clone(),
                });

                let array = hir_def_expr(*array, env, &ty, parent);

                HDefExpr {
                    ident: array.ident.clone(),
                    var_ty: array.var_ty.clone(),
                    expr_ty: ty,
                    kind: HDefExprKind::Subscript {
                        array,
                        index: hir_val_expr(*index, env),
                        bracket,
                    },
                }
            }
            _ => todo!("recover"),
        },
    })
}

fn hir_ident(ast: AIdent, env: &mut Env) -> HN<HIdent> {
    let AIdent { token } = ast;
    HN::new(HIdent { token })
}

fn hir_stmt(ast: AStmt, env: &mut Env) -> HN<HStmt> {
    HN::new(match ast {
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
                .collect();

            HStmt {
                defs: args.iter().map(|def| def.expr.clone()).collect(),
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
                defs: Vec::new(),
                kind: HStmtKind::Write {
                    inst,
                    args: args
                        .into_pairs()
                        .map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                hir_val_expr(a, env)
                            }
                            syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                        })
                        .collect(),
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
                Some((a, r)) => (Some(a), Some(hir_def(r, env))),
                None => (None, None),
            };

            HStmt {
                defs: ret.iter().map(|r| r.expr.clone()).collect(),
                kind: HStmtKind::Call {
                    inst,
                    name: hir_ident(name, env),
                    args_paren,
                    args: args
                        .into_pairs()
                        .map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                hir_val_expr(a, env)
                            }
                            syn::punctuated::Pair::End(a) => hir_val_expr(a, env),
                        })
                        .collect(),
                    arg_commas,
                    ret_rarrow,
                    ret,
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
            let index_name = hir_ident(index_name, env);
            let range = HN::new(HRange {
                bound: hir_val_expr(bound, env),
                upto,
                index_name: index_name.clone(),
            });

            let mut inner_env = Env {
                refs: vec![HN::new(HRef {
                    ident: index_name,
                    kind: HRefKind::Index {
                        range: range.clone(),
                    },
                })],
                outer: Some(Box::new((*env).clone())),
                cons_path: ConsPath::For {
                    range: range.clone(),
                    parent: Box::new(env.cons_path.clone()),
                },
            };

            let body = hir_block(body, &mut inner_env);

            HStmt {
                defs: body
                    .defs
                    .iter()
                    .map(|expr| match &expr.kind {
                        HDefExprKind::Subscript { array, .. } => array.clone(),
                        _ => todo!("recover"),
                    })
                    .collect(),
                kind: HStmtKind::For {
                    for_token,
                    range,
                    body_brace,
                    body,
                },
            }
        }
    })
}

fn hir_atom_ty(ast: ATy, env: &mut Env) -> HN<HAtomTy> {
    HN::new(HAtomTy {
        ident: hir_ident(ast.ident, env),
    })
}

pub fn compile_hir(ast: ASpec) -> HSpec {
    HSpec {
        main: hir_block(
            ast.main,
            &mut Env {
                refs: Vec::new(),
                outer: None,
                cons_path: ConsPath::Root,
            },
        ),
    }
}
