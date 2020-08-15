use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;
use crate::hir_env::*;

fn hir_block(ast: ABlock, env: &Env) -> HN<HBlock> {
    let mut env = env.clone();
    let mut stmts = Vec::new();

    for stmt in ast.stmts {
        let stmt = hir_stmt(stmt, &env);
        env.refs.extend(stmt.decls.iter().cloned());
        stmts.push(stmt);
    }

    HN::new(HBlock {
        decls: stmts
            .iter()
            .flat_map(|stmt| stmt.decls.iter())
            .cloned()
            .collect(),
        defs: stmts
            .iter()
            .flat_map(|stmt| stmt.defs.iter())
            .cloned()
            .collect(),
        stmts,
    })
}

fn hir_def(ast: ADef, env: &Env) -> HN<HDef> {
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

    HN::new(HDef {
        colon,
        atom_ty,
        ident: expr.ident.clone(),
        var_ty: expr.expr_ty.clone(),
        expr,
    })
}

fn hir_val_expr(ast: AExpr, env: &Env) -> HN<HValExpr> {
    HN::new(match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident, env);
            let target = env.resolve(ident.clone());

            let ty = match &target.as_ref().unwrap_or_else(|| todo!("recover")).kind {
                HDeclKind::Var { def } => def.var_ty.clone(),
                HDeclKind::Index { range } => HN::new(HExprTy::Index {
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

fn hir_def_expr(ast: AExpr, env: &Env, ty: &HN<HExprTy>, cons_path: &ConsPath) -> HN<HDefExpr> {
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

fn hir_ident(ast: AIdent, env: &Env) -> HN<HIdent> {
    let AIdent { token } = ast;
    HN::new(HIdent { token })
}

fn hir_stmt(ast: AStmt, env: &Env) -> HN<HStmt> {
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
                decls: args.iter().map(hir_def_decl).collect(),
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
                decls: Vec::new(),
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
                decls: ret.iter().map(hir_def_decl).collect(),
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
                refs: vec![HN::new(HDecl {
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
            };

            let body = hir_block(body, &mut inner_env);

            HStmt {
                decls: body.decls.clone(),
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

fn hir_atom_ty(ast: ATy, env: &Env) -> HN<HAtomTy> {
    HN::new(HAtomTy {
        ident: hir_ident(ast.ident, env),
    })
}

fn hir_def_decl(def: &HN<HDef>) -> HN<HDecl> {
    HN::new(HDecl {
        ident: def.ident.clone(),
        kind: HDeclKind::Var { def: def.clone() },
    })
}

pub fn compile_hir(ast: ASpec) -> HSpec {
    HSpec {
        main: hir_block(
            ast.main,
            &Env {
                refs: Vec::new(),
                outer: None,
                cons_path: ConsPath::Root,
            },
        ),
    }
}
