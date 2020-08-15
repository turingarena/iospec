use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;

#[derive(Debug, Clone)]
struct Env {
    refs: Vec<HN<HRef>>,
    outer: Option<Scope>,
}

impl Env {
    fn resolve(self: &Self, ident: HN<HIdent>) -> Option<HN<HRef>> {
        self.refs
            .iter()
            .find(|r| r.ident.token == ident.token)
            .map(|r| r.clone())
            .or(self.outer.as_ref().and_then(|s| s.resolve(ident)))
    }
}

#[derive(Debug, Clone)]
enum Scope {
    For { range: HN<HRange>, env: Box<Env> },
}

impl Scope {
    fn resolve(self: &Self, ident: HN<HIdent>) -> Option<HN<HRef>> {
        match self {
            Scope::For { range, env } => {
                // TODO: resolve index
                env.resolve(ident)
            }
        }
    }
}

fn hir_block(ast: ABlock, env: &mut Env) -> HN<HBlock> {
    let stmts: Vec<HN<HStmt>> = ast
        .stmts
        .into_iter()
        .map(|stmt| hir_stmt(stmt, env))
        .collect();
    HN::new(HBlock {
        conses: stmts
            .iter()
            .flat_map(|stmt| stmt.conses.iter())
            .cloned()
            .collect(),
        stmts,
    })
}

fn hir_def(ast: ADef, env: &mut Env) -> HN<HDef> {
    let ADef { expr, colon, ty } = ast;
    let def = HN::new(HDef {
        expr: hir_def_expr(expr, env),
        colon,
        ty: hir_scalar_type_expr(ty, env),
    });
    env.refs.push(HN::new(HRef {
        ident: def.expr.ident.clone(),
        kind: HRefKind::Var {
            def: def.clone(),
            cons: HN::new(HCons::Scalar { def: def.clone() }),
        },
    }));
    def
}

fn hir_expr(ast: AExpr, env: &mut Env) -> HN<HExpr> {
    HN::new(match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident, env);
            HExpr {
                kind: HExprKind::Ref {
                    target: env.resolve(ident.clone()),
                    ident,
                },
            }
        }
        AExpr::Subscript {
            array,
            bracket,
            index,
        } => HExpr {
            kind: HExprKind::Subscript {
                array: hir_expr(*array, env),
                index: hir_expr(*index, env),
                bracket,
            },
        },
    })
}

fn hir_def_expr(ast: AExpr, env: &mut Env) -> HN<HDefExpr> {
    HN::new(match ast {
        AExpr::Ref { ident } => {
            let ident = hir_ident(ident, env);
            HDefExpr {
                ident: ident.clone(),
                kind: HDefExprKind::Var { ident },
            }
        }
        AExpr::Subscript {
            array,
            bracket,
            index,
        } => {
            let array = hir_def_expr(*array, env);
            HDefExpr {
                ident: array.ident.clone(),
                kind: HDefExprKind::Subscript {
                    array,
                    index: hir_expr(*index, env),
                    bracket,
                },
            }
        }
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
                conses: args
                    .iter()
                    .map(|def| HN::new(HCons::Scalar { def: def.clone() }))
                    .collect(),
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
                conses: Vec::new(),
                kind: HStmtKind::Write {
                    inst,
                    args: args
                        .into_pairs()
                        .map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                hir_expr(a, env)
                            }
                            syn::punctuated::Pair::End(a) => hir_expr(a, env),
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
                conses: ret
                    .iter()
                    .map(|r| HN::new(HCons::Scalar { def: r.clone() }))
                    .collect(),
                kind: HStmtKind::Call {
                    inst,
                    name: hir_ident(name, env),
                    args_paren,
                    args: args
                        .into_pairs()
                        .map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                hir_expr(a, env)
                            }
                            syn::punctuated::Pair::End(a) => hir_expr(a, env),
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
            let range = HN::new(HRange {
                bound: hir_expr(bound, env),
                upto,
                index_name: hir_ident(index_name, env),
            });

            let mut inner_env = Env {
                refs: Vec::new(),
                outer: Some(Scope::For {
                    range: range.clone(),
                    env: Box::new((*env).clone()),
                }),
            };
            let body = hir_block(body, &mut inner_env);

            HStmt {
                conses: body
                    .conses
                    .iter()
                    .map(|c| {
                        HN::new(HCons::Array {
                            item: c.clone(),
                            range: range.clone(),
                        })
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

fn hir_scalar_type_expr(ast: ATy, env: &mut Env) -> HN<HScalarTypeExpr> {
    HN::new(HScalarTypeExpr {
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
            },
        ),
    }
}
