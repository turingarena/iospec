//! Transform an AST into HIR.
//!
//! Transformation is done by traversing the AST tree in post-order and generating HIR nodes,
//! while keeping and updating the environment (see `hir_env`) of each AST node
//! encountered in the traversal.
//! At the end the environment is discarded, and only the HIR (with its internal links) is kept.

use std::ops::Deref;

use crate::ast::*;
use crate::diagnostic::*;
use crate::hir::*;
use crate::hir_env::*;
use crate::hir_err::*;
use crate::ty::*;

trait HirCompileFrom<T, E = Env> {
    fn compile(ast: T, env: &E, dgns: &mut Vec<Diagnostic>) -> Self;
}

impl<T, U, E> HirCompileFrom<T, E> for Rc<U>
where
    U: HirCompileFrom<T, E>,
{
    fn compile(ast: T, env: &E, dgns: &mut Vec<Diagnostic>) -> Self {
        Rc::new(U::compile(ast, env, dgns))
    }
}

trait HirCompileInto<T, E = Env> {
    fn compile(self: Self, env: &E, dgns: &mut Vec<Diagnostic>) -> T;
}

impl<U, T, E> HirCompileInto<U, E> for T
where
    U: HirCompileFrom<T, E>,
{
    fn compile(self: Self, env: &E, dgns: &mut Vec<Diagnostic>) -> U {
        U::compile(self, env, dgns)
    }
}

impl<T: HirCompileInto<HStepExpr>> HirCompileFrom<T> for HStep {
    fn compile(ast: T, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let expr: HStepExpr = ast.compile(env, dgns);

        HStep {
            nodes: match &expr {
                HStepExpr::Seq { steps } => {
                    steps.iter().flat_map(|s| s.nodes.iter()).cloned().collect()
                }
                HStepExpr::Read { args, .. } => args.iter().map(|d| d.node.clone()).collect(),
                HStepExpr::Call { fun, .. } => fun.ret.iter().map(|d| d.node.clone()).collect(),
                HStepExpr::For { body, .. } => body
                    .nodes
                    .iter()
                    .flat_map(|node| match &node.expr {
                        HNodeDefExpr::Subscript { array, .. } => Some(array.clone()),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            },
            funs: match &expr {
                HStepExpr::Seq { steps } => {
                    steps.iter().flat_map(|s| s.funs.iter()).cloned().collect()
                }
                HStepExpr::Call { fun, .. } => vec![fun.clone()],
                HStepExpr::For { body, .. } => body.funs.clone(),
                _ => Vec::new(),
            },
            expr,
        }
    }
}

impl HirCompileFrom<ABlock> for HStepExpr {
    fn compile(ast: ABlock, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let mut env = env.clone();
        let mut steps = Vec::new();

        for stmt in ast.stmts {
            let stmt: Rc<HStep> = stmt.compile(&env, dgns);
            for node in stmt.nodes.iter() {
                let var = &node.root_var;
                match &var.expr {
                    HVarDefExpr::Name { name } => env.declare(
                        &Rc::new(HVar {
                            expr: HVarExpr::Data { def: var.clone() },
                            ty: var.ty.clone(),
                            name: name.clone(),
                        }),
                        dgns,
                    ),
                    _ => (),
                };
            }
            steps.push(stmt);
        }

        HStepExpr::Seq { steps }
    }
}

impl HirCompileFrom<AStmt> for HStepExpr {
    fn compile(ast: AStmt, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        match ast {
            AStmt::Read { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStepExpr::Read {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env, dgns)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Write { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStepExpr::Write {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env, dgns)).collect(),
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
                    Some((a, r)) => (Some(a), Some(r.compile(env, dgns))),
                    None => (None, None),
                };

                HStepExpr::Call {
                    kw,
                    fun: Rc::new(HFun {
                        name: name.compile(&(), dgns),
                        args: args.into_iter().map(|a| a.compile(env, dgns)).collect(),
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
                    index: index.compile(&(), dgns),
                    upto,
                    bound: bound.compile(env, dgns),
                });

                HStepExpr::For {
                    kw,
                    body: body.compile(&env.for_body(range.clone()), dgns),
                    range,
                    body_brace,
                }
            }
        }
    }
}

impl HirCompileFrom<ADef> for HAtomDef {
    fn compile(ast: ADef, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let ADef { expr, colon, ty } = ast;
        let ty: Rc<HAtomTy> = ty.compile(&(), dgns);

        HAtomDef {
            colon,
            node: expr.compile(
                &HDefEnv {
                    env: env.clone(),
                    ty: Rc::new(HValTy::Atom {
                        atom_ty: ty.clone(),
                    }),
                    loc: env.loc.clone(),
                },
                dgns,
            ),
            ty,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HNodeDef {
    fn compile(ast: AExpr, env: &HDefEnv, dgns: &mut Vec<Diagnostic>) -> Self {
        let expr: HNodeDefExpr = ast.compile(env, dgns);

        HNodeDef {
            ty: env.ty.clone(),
            root_var: match &expr {
                HNodeDefExpr::Var { var } => var.clone(),
                HNodeDefExpr::Subscript { array, .. } => array.root_var.clone(),
                HNodeDefExpr::Err => HErr::err(),
            },
            expr,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HNodeDefExpr {
    fn compile(ast: AExpr, env: &HDefEnv, dgns: &mut Vec<Diagnostic>) -> Self {
        match ast {
            AExpr::Ref { ident } => HNodeDefExpr::Var {
                var: ident.compile(env, dgns),
            },
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => {
                let index: Rc<HVal> = (*index).compile(&env.env, &mut Vec::new()); // ignore diagnostics

                match env.loc.deref() {
                    HDataLoc::For {
                        range: expected_range,
                        parent,
                    } => match &index.expr {
                        HValExpr::Var { name, var } => match &var.expr {
                            HVarExpr::Index {
                                range: actual_range,
                            } => {
                                if Rc::ptr_eq(&expected_range, &actual_range) {
                                    HNodeDefExpr::Subscript {
                                        array: (*array).compile(
                                            &HDefEnv {
                                                env: env.env.clone(),
                                                ty: Rc::new(HValTy::Array {
                                                    item: env.ty.clone(),
                                                    range: expected_range.clone(),
                                                }),
                                                loc: parent.clone(),
                                            },
                                            dgns,
                                        ),
                                        bracket,
                                        index,
                                    }
                                } else {
                                    dgns.push(Diagnostic::SubscriptDefIndexNotMatched {
                                        bracket: bracket.clone(),
                                        expected_range: Some(expected_range.clone()),
                                        actual_range: Some(actual_range.clone()),
                                        name: Some(name.clone()),
                                    });
                                    HErr::err()
                                }
                            }
                            _ => {
                                dgns.push(Diagnostic::SubscriptDefIndexNotMatched {
                                    expected_range: Some(expected_range.clone()),
                                    actual_range: None,
                                    bracket: bracket.clone(),
                                    name: None,
                                });
                                HErr::err()
                            }
                        },
                        _ => {
                            dgns.push(Diagnostic::SubscriptDefIndexNotMatched {
                                expected_range: Some(expected_range.clone()),
                                actual_range: None,
                                bracket: bracket.clone(),
                                name: None,
                            });
                            HErr::err()
                        }
                    },
                    _ => {
                        dgns.push(Diagnostic::SubscriptDefIndexNotMatched {
                            expected_range: None,
                            actual_range: None,
                            bracket: bracket.clone(),
                            name: None,
                        });
                        HErr::err()
                    }
                }
            }
        }
    }
}

impl HirCompileFrom<AIdent, HDefEnv> for HVarDef {
    fn compile(ast: AIdent, env: &HDefEnv, dgns: &mut Vec<Diagnostic>) -> Self {
        HVarDef {
            expr: HVarDefExpr::Name {
                name: ast.compile(&(), dgns),
            },
            ty: env.ty.clone(),
        }
    }
}

impl HirCompileFrom<AExpr> for HArg {
    fn compile(ast: AExpr, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let val: Rc<HVal> = ast.compile(env, dgns);

        HArg {
            val: val.clone(),
            expr: match &val.expr {
                HValExpr::Var { var, .. } => HArgExpr::Name {
                    name: var.name.clone(),
                },
                _ => {
                    dgns.push(Diagnostic::ArgumentNotVariable { val: val.clone() });
                    HErr::err()
                }
            },
        }
    }
}

impl HirCompileFrom<AExpr> for HVal {
    fn compile(ast: AExpr, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let expr: HValExpr = ast.compile(env, dgns);

        HVal {
            ty: match &expr {
                HValExpr::Var { var, .. } => var.ty.clone(),
                HValExpr::Subscript {
                    array,
                    index,
                    bracket,
                } => match array.ty.deref() {
                    HValTy::Array { item, range } => {
                        match index.ty.deref() {
                            HValTy::Atom { atom_ty } if atom_ty.sem == range.bound.ty.sem => (),
                            _ => dgns.push(Diagnostic::SubscriptIndexWrongType {
                                range: range.clone(),
                                array: array.clone(),
                                index: index.clone(),
                                bracket: bracket.clone(),
                            }),
                        }
                        item.clone()
                    }
                    HValTy::Err => HErr::err(),
                    _ => {
                        dgns.push(Diagnostic::SubscriptArrayNotArray {
                            array: array.clone(),
                            index: index.clone(),
                            bracket: bracket.clone(),
                        });

                        HErr::err()
                    }
                },
            },
            expr,
        }
    }
}

impl HirCompileFrom<AExpr> for HRangeBound {
    fn compile(ast: AExpr, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let val: Rc<HVal> = ast.compile(env, dgns);

        match val.ty.deref() {
            HValTy::Atom { atom_ty } => {
                if let AtomTy::Natural { .. } = &atom_ty.sem {
                    // OK
                } else {
                    dgns.push(Diagnostic::RangeBoundNotNatural {
                        val: val.clone(),
                        atom_ty: Some(atom_ty.clone()),
                    })
                }

                HRangeBound {
                    ty: atom_ty.clone(),
                    val,
                }
            }
            _ => {
                dgns.push(Diagnostic::RangeBoundNotNatural {
                    val: val.clone(),
                    atom_ty: None,
                });
                HRangeBound {
                    ty: HErr::err(),
                    val,
                }
            }
        }
    }
}

impl HirCompileFrom<AExpr> for HAtom {
    fn compile(ast: AExpr, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        let val: Rc<HVal> = ast.compile(env, dgns);

        HAtom {
            ty: match val.ty.deref() {
                HValTy::Atom { atom_ty } => atom_ty.clone(),
                HValTy::Err => HErr::err(),
                _ => {
                    dgns.push(Diagnostic::AtomNotScalar { val: val.clone() });
                    HErr::err()
                }
            },
            val,
        }
    }
}

impl HirCompileFrom<AExpr> for HValExpr {
    fn compile(ast: AExpr, env: &Env, dgns: &mut Vec<Diagnostic>) -> Self {
        match ast {
            AExpr::Ref { ident } => {
                let name = ident.compile(&(), dgns);
                let var = env.resolve(&name, dgns);

                HValExpr::Var { var, name }
            }
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => HValExpr::Subscript {
                array: (*array).compile(env, dgns),
                index: (*index).compile(env, dgns),
                bracket,
            },
        }
    }
}

impl HirCompileFrom<ATy, ()> for HAtomTy {
    fn compile(ast: ATy, _: &(), dgns: &mut Vec<Diagnostic>) -> Self {
        let name: Rc<HName> = ast.ident.compile(&(), dgns);

        HAtomTy {
            sem: AtomTy::all()
                .into_iter()
                .find(|k| k.name() == name.ident.to_string())
                .unwrap_or_else(|| {
                    dgns.push(Diagnostic::InvalidAtomTy {
                        ident: name.clone(),
                    });
                    AtomTy::Err
                }),
            expr: HAtomTyExpr::Name { name },
        }
    }
}

impl HirCompileFrom<AIdent, ()> for HName {
    fn compile(ast: AIdent, _: &(), _dgns: &mut Vec<Diagnostic>) -> Self {
        let AIdent { token } = ast;
        HName { ident: token }
    }
}

pub fn compile_hir(ast: ASpec, dgns: &mut Vec<Diagnostic>) -> Result<Rc<HSpec>, ()> {
    let main: Rc<HStep> = ast.main.compile(&Env::main(), dgns);

    if dgns.iter().any(|d| d.is_critical()) {
        Err(())?
    }

    Ok(Rc::new(HSpec {
        funs: main.funs.clone(),
        main,
    }))
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
