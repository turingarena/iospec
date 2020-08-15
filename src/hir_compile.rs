use std::ops::Deref;

use crate::ast::*;
use crate::hir::*;

#[derive(Debug, Clone)]
struct Env {
    refs: Vec<HirNode<HirRef>>,
    outer: Option<Scope>,
}

impl Env {
    fn resolve(self: &Self, ident: HirNode<HirIdent>) -> Option<HirNode<HirRef>> {
        self.refs.iter().find(|r| r.ident.token == ident.token).map(|r| r.clone()).or(
            self.outer.as_ref().and_then(|s| s.resolve(ident))
        )
    }
}

#[derive(Debug, Clone)]
enum Scope {
    For {
        range: HirNode<HirRange>,
        env: Box<Env>,
    },
}

impl Scope {
    fn resolve(self: &Self, ident: HirNode<HirIdent>) -> Option<HirNode<HirRef>> {
        match self {
            Scope::For { range, env } => {
                // TODO: resolve index
                env.resolve(ident)
            }
        }
    }
}

impl AstBlock {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirBlock> {
        let stmts: Vec<HirNode<HirStmt>> = self.stmts.into_iter().map(|stmt| stmt.compile(env)).collect();
        HirNode::new(HirBlock {
            conses: stmts.iter().flat_map(|stmt| stmt.conses.iter()).map(Clone::clone).collect(),
            stmts,
        })
    }
}

impl AstDef {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirDef> {
        let AstDef { expr, colon, ty } = self;
        let def = HirNode::new(HirDef {
            expr: expr.compile_def(env),
            colon,
            ty: ty.compile(env),
        });
        env.refs.push(HirNode::new(HirRef {
            ident: def.expr.ident.clone(),
            kind: HirRefKind::Var {
                cons: HirNode::new(HirCons::Scalar {
                    def: def.clone(),
                }),
            },
        }));
        def
    }
}

impl AstExpr {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirExpr> {
        HirNode::new(match self {
            AstExpr::Ref { ident } => {
                let ident = ident.compile(env);
                HirExpr {
                    kind: HirExprKind::Ref {
                        binding: env.resolve(ident.clone()),
                        ident,
                    }
                }
            }
            AstExpr::Subscript { array, bracket, index } => HirExpr {
                kind: HirExprKind::Subscript {
                    array: array.compile(env),
                    index: index.compile(env),
                    bracket,
                }
            },
        })
    }

    fn compile_def(self: AstExpr, env: &mut Env) -> HirNode<HirDefExpr> {
        HirNode::new(match self {
            AstExpr::Ref { ident } => {
                let ident = ident.compile(env);
                HirDefExpr {
                    ident: ident.clone(),
                    kind: HirDefExprKind::Var {
                        ident,
                    },
                }
            }
            AstExpr::Subscript { array, bracket, index } => {
                let array = array.compile_def(env);
                HirDefExpr {
                    ident: array.ident.clone(),
                    kind: HirDefExprKind::Subscript {
                        array,
                        index: index.compile(env),
                        bracket,
                    },
                }},
        })
    }
}

impl AstIdent {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirIdent> {
        let AstIdent { token } = self;
        HirNode::new(HirIdent { token })
    }
}

impl AstStmt {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirStmt> {
        HirNode::new(match self {
            AstStmt::Read {
                inst, args, semi,
            } => {
                let mut arg_commas = Vec::new();

                let args: Vec<HirNode<HirDef>> = args.into_pairs().map(|a| match a {
                    syn::punctuated::Pair::Punctuated(a, comma) => {
                        arg_commas.push(comma);
                        a.compile(env)
                    }
                    syn::punctuated::Pair::End(a) => a.compile(env),
                }).collect();

                HirStmt {
                    conses: args.iter().map(|def| HirNode::new(HirCons::Scalar {
                        def: def.clone(),
                    })).collect(),
                    kind: HirStmtKind::Read {
                        inst,
                        args,
                        arg_commas,
                        semi,
                    }
                }
            }
            AstStmt::Write {
                inst, args, semi,
            } => {
                let mut arg_commas = Vec::new();

                HirStmt {
                    conses: Vec::new(),
                    kind: HirStmtKind::Write {
                        inst,
                        args: args.into_pairs().map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                a.compile(env)
                            }
                            syn::punctuated::Pair::End(a) => a.compile(env),
                        }).collect(),
                        arg_commas,
                        semi,
                    }
                }
            }
            AstStmt::Call {
                inst, name, args_paren, args, return_value, semi
            } => {
                let mut arg_commas = Vec::new();
                let return_value: Option<(_, HirNode<HirDef>)> = return_value.map(|(arrow, r)| (arrow, r.compile(env)));

                HirStmt {
                    conses: return_value.iter().map(|(_, r)| HirNode::new(HirCons::Scalar {
                        def: r.clone(),
                    })).collect(),
                    kind: HirStmtKind::Call {
                        inst,
                        name: name.compile(env),
                        args_paren,
                        args: args.into_pairs().map(|a| match a {
                            syn::punctuated::Pair::Punctuated(a, comma) => {
                                arg_commas.push(comma);
                                a.compile(env)
                            }
                            syn::punctuated::Pair::End(a) => a.compile(env),
                        }).collect(),
                        arg_commas,
                        return_value,
                        semi,
                    }
                }
            }
            AstStmt::For {
                for_token, index_name, upto, bound, body_brace, body
            } => {
                let range = HirNode::new(HirRange {
                    bound: bound.compile(env),
                    upto,
                    index_name: index_name.compile(env),
                });

                let mut inner_env = Env {
                    refs: Vec::new(),
                    outer: Some(Scope::For {
                        range: range.clone(),
                        env: Box::new((*env).clone()),
                    }),
                };
                let body = body.compile(&mut inner_env);

                HirStmt {
                    conses: body.conses.iter().map(|c| HirNode::new(HirCons::Array {
                        item: c.clone(),
                        range: range.clone(),
                    })).collect(),
                    kind: HirStmtKind::For {
                        for_token,
                        range,
                        body_brace,
                        body,
                    }
                }
            }
        })
    }
}

impl AstScalarTypeExpr {
    fn compile(self: Self, env: &mut Env) -> HirNode<HirScalarTypeExpr> {
        HirNode::new(HirScalarTypeExpr {
            ident: self.ident.compile(env),
        })
    }
}

pub fn compile_hir(ast: AstSpec) -> HirSpec {
    HirSpec {
        main: ast.main.compile(&mut Env {
            refs: Vec::new(),
            outer: None,
        })
    }
}