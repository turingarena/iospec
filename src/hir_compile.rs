use crate::ast::*;
use crate::hir::*;
use std::ops::Deref;

#[derive(Debug, Clone)]
enum Scope {
    For {
        range: HirNode<HirRange>,
        env: Box<Env>,
    },
}

#[derive(Debug, Clone)]
struct Env {
    defs: Vec<HirNode<HirDef>>,
    outer: Option<Scope>,
}

trait HirCompile<T>: std::marker::Sized {
    fn compile(ast: T, env: &mut Env) -> HirNode<Self>;
}

impl<T, U> HirCompile<Box<T>> for U
    where U: HirCompile<T>
{
    fn compile(ast: Box<T>, env: &mut Env) -> HirNode<Self> {
        (*ast).compile(env)
    }
}

trait HirCompileInto<U> {
    fn compile(self: Self, env: &mut Env) -> HirNode<U>;
}

impl<T, U> HirCompileInto<U> for T
    where
        U: HirCompile<T> {
    fn compile(self: Self, env: &mut Env) -> HirNode<U> {
        U::compile(self, env)
    }
}

impl HirCompile<AstBlock> for HirBlock {
    fn compile(ast: AstBlock, env: &mut Env) -> HirNode<Self> {
        HirNode::new(HirBlock {
            stmts: ast.stmts.into_iter().map(|stmt| stmt.compile(env)).collect(),
        })
    }
}

impl HirCompile<AstDef> for HirDef {
    fn compile(ast: AstDef, env: &mut Env) -> HirNode<Self> {
        let AstDef { expr, colon, ty } = ast;
        HirNode::new(HirDef {
            expr: expr.compile(env),
            colon,
            ty: ty.compile(env),
        })
    }
}

impl HirCompile<AstIdent> for HirIdent {
    fn compile(ast: AstIdent, env: &mut Env) -> HirNode<Self> {
        let AstIdent { sym } = ast;
        HirNode::new(HirIdent { sym })
    }
}

impl HirCompile<AstStmt> for HirStmt {
    fn compile(ast: AstStmt, env: &mut Env) -> HirNode<Self> {
        HirNode::new(match ast {
            AstStmt::Read {
                inst, args, semi,
            } => {
                let mut arg_commas = Vec::new();

                HirStmt {
                    kind: HirStmtKind::Read {
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
            AstStmt::Write {
                inst, args, semi,
            } => {
                let mut arg_commas = Vec::new();

                HirStmt {
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

                HirStmt {
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
                        return_value: return_value.map(|(arrow, r)| (arrow, r.compile(env))),
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
                    defs: Vec::new(),
                    outer: Some(Scope::For {
                        range: range.clone(),
                        env: Box::new((*env).clone()),
                    })
                };

                HirStmt {
                    kind: HirStmtKind::For {
                        for_token,
                        range,
                        body_brace,
                        body: body.compile(&mut inner_env),
                    }
                }
            }
        })
    }
}

impl HirCompile<AstExpr> for HirExpr {
    fn compile(ast: AstExpr, env: &mut Env) -> HirNode<Self> {
        HirNode::new(match ast {
            AstExpr::Ref { ident } => HirExpr {
                kind: HirExprKind::Ref {
                    ident: ident.compile(env),
                }
            },
            AstExpr::Subscript { array, bracket, index } => HirExpr {
                kind: HirExprKind::Subscript {
                    array: array.compile(env),
                    index: index.compile(env),
                    bracket,
                }
            },
        })
    }
}

impl HirCompile<AstScalarTypeExpr> for HirScalarTypeExpr {
    fn compile(ast: AstScalarTypeExpr, env: &mut Env) -> HirNode<Self> {
        HirNode::new(HirScalarTypeExpr {
            ident: ast.ident.compile(env),
        })
    }
}

pub fn compile_hir(ast: AstSpec) -> HirSpec {
    HirSpec {
        main: ast.main.compile(&mut Env {
            defs: Vec::new(),
            outer: None,
        })
    }
}