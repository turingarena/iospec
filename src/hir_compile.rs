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
    fn compile(ast: T, env: &E, sess: &mut Sess) -> Self;
}

impl<T, U, E> HirCompileFrom<T, E> for Rc<U>
where
    U: HirCompileFrom<T, E>,
{
    fn compile(ast: T, env: &E, sess: &mut Sess) -> Self {
        Rc::new(U::compile(ast, env, sess))
    }
}

trait HirCompileInto<T, E = Env> {
    fn compile(self: Self, env: &E, sess: &mut Sess) -> T;
}

impl<U, T, E> HirCompileInto<U, E> for T
where
    U: HirCompileFrom<T, E>,
{
    fn compile(self: Self, env: &E, sess: &mut Sess) -> U {
        U::compile(self, env, sess)
    }
}

impl HStep {
    fn funs(self: &Self) -> Vec<Rc<HFun>> {
        match self.expr.deref() {
            HStepExpr::Seq { steps } => steps.iter().flat_map(|s| s.funs()).collect(),
            HStepExpr::Call { fun, .. } => vec![fun.clone()],
            HStepExpr::For { body, .. } => body.funs(),
            _ => Vec::new(),
        }
    }
}

impl<T: HirCompileInto<Rc<HStepExpr>>> HirCompileFrom<T> for HStep {
    fn compile(ast: T, env: &Env, sess: &mut Sess) -> Self {
        let expr: Rc<HStepExpr> = ast.compile(env, sess);
        HStep {
            nodes: match expr.deref() {
                HStepExpr::Seq { steps } => {
                    steps.iter().flat_map(|s| s.nodes.iter()).cloned().collect()
                }
                HStepExpr::Read { args, .. } => args.iter().map(|d| d.node.clone()).collect(),
                HStepExpr::Call { fun, .. } => fun.ret.iter().map(|d| d.node.clone()).collect(),
                HStepExpr::For { body, .. } => body
                    .nodes
                    .iter()
                    .flat_map(|node| match node.expr.deref() {
                        HNodeDefExpr::Subscript { array, .. } => Some(array.clone()),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            },
            expr,
        }
    }
}

impl HirCompileFrom<ABlock> for HStepExpr {
    fn compile(ast: ABlock, env: &Env, sess: &mut Sess) -> Self {
        let mut env = env.clone();
        let mut steps = Vec::new();

        for stmt in ast.stmts {
            let stmt: Rc<HStep> = stmt.compile(&env, sess);
            for node in stmt.nodes.iter() {
                let var = &node.root_var;
                match var.expr.deref() {
                    HVarDefExpr::Name { name } => env.declare(
                        &Rc::new(HVar {
                            expr: Rc::new(HVarExpr::Data { def: var.clone() }),
                            ty: var.ty.clone(),
                            name: name.clone(),
                        }),
                        sess,
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
    fn compile(ast: AStmt, env: &Env, sess: &mut Sess) -> Self {
        match ast {
            AStmt::Read { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStepExpr::Read {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env, sess)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Write { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStepExpr::Write {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env, sess)).collect(),
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
                    Some((a, r)) => (Some(a), Some(r.compile(env, sess))),
                    None => (None, None),
                };

                HStepExpr::Call {
                    kw,
                    fun: Rc::new(HFun {
                        name: name.compile(&(), sess),
                        args: args.into_iter().map(|a| a.compile(env, sess)).collect(),
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
                    index: index.compile(&(), sess),
                    upto,
                    bound: bound.compile(env, sess),
                });

                HStepExpr::For {
                    kw,
                    body: body.compile(&env.for_body(range.clone()), sess),
                    range,
                    body_brace,
                }
            }
        }
    }
}

impl HirCompileFrom<ADef> for HAtomDef {
    fn compile(ast: ADef, env: &Env, sess: &mut Sess) -> Self {
        let ADef { expr, colon, ty } = ast;
        let ty: Rc<HAtomTy> = ty.compile(&(), sess);

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
                sess,
            ),
            ty,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HNodeDef {
    fn compile(ast: AExpr, env: &HDefEnv, sess: &mut Sess) -> Self {
        let expr: Rc<HNodeDefExpr> = ast.compile(env, sess);

        HNodeDef {
            ty: env.ty.clone(),
            root_var: match expr.deref() {
                HNodeDefExpr::Var { var } => var.clone(),
                HNodeDefExpr::Subscript { array, .. } => array.root_var.clone(),
                HNodeDefExpr::Err => HErr::err(),
            },
            var: match expr.deref() {
                HNodeDefExpr::Var { var } => Some(var.clone()),
                _ => None,
            },
            expr,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HNodeDefExpr {
    fn compile(ast: AExpr, env: &HDefEnv, sess: &mut Sess) -> Self {
        match ast {
            AExpr::Ref { ident } => HNodeDefExpr::Var {
                var: ident.compile(env, sess),
            },
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => match env.loc.deref() {
                HDataLoc::For { range, parent } => match *index {
                    AExpr::Ref { ident } => {
                        let name: Rc<HName> = ident.compile(&(), sess);

                        if name.to_string() == range.index.to_string() {
                            let index = Rc::new(HIndex {
                                name: name.clone(),
                                range: range.clone(),
                            });

                            HNodeDefExpr::Subscript {
                                array: (*array).compile(
                                    &HDefEnv {
                                        env: env.env.clone(),
                                        ty: Rc::new(HValTy::Array {
                                            item: env.ty.clone(),
                                            range: range.clone(),
                                        }),
                                        loc: parent.clone(),
                                    },
                                    sess,
                                ),
                                bracket,
                                index,
                            }
                        } else {
                            sess.diagnostics
                                .push(Diagnostic::SubscriptDefIndexNotMatched {
                                    bracket: bracket.clone(),
                                    range: Some(range.clone()),
                                    name: Some(name.clone()),
                                });
                            HErr::err()
                        }
                    }
                    _ => {
                        sess.diagnostics
                            .push(Diagnostic::SubscriptDefIndexNotMatched {
                                range: Some(range.clone()),
                                bracket: bracket.clone(),
                                name: None,
                            });
                        HErr::err()
                    }
                },
                _ => {
                    sess.diagnostics
                        .push(Diagnostic::SubscriptDefIndexNotMatched {
                            range: None,
                            bracket: bracket.clone(),
                            name: None,
                        });
                    HErr::err()
                }
            },
        }
    }
}

impl HirCompileFrom<AIdent, HDefEnv> for HVarDef {
    fn compile(ast: AIdent, env: &HDefEnv, sess: &mut Sess) -> Self {
        HVarDef {
            expr: Rc::new(HVarDefExpr::Name {
                name: ast.compile(&(), sess),
            }),
            ty: env.ty.clone(),
        }
    }
}

impl HValExpr {
    fn ty(self: &Self, sess: &mut Sess) -> Rc<HValTy> {
        match self {
            HValExpr::Var { var, .. } => var.ty.clone(),
            HValExpr::Subscript {
                array,
                index,
                bracket,
            } => match array.ty.deref() {
                HValTy::Array { item, range } => {
                    match index.ty.deref() {
                        HValTy::Atom { atom_ty } => {
                            if atom_ty.sem != range.bound.ty.sem {
                                sess.diagnostics.push(Diagnostic::SubscriptIndexWrongType {
                                    range: range.clone(),
                                    array: array.clone(),
                                    index: index.clone(),
                                    bracket: bracket.clone(),
                                })
                            }
                        }
                        _ => sess.diagnostics.push(Diagnostic::SubscriptIndexWrongType {
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
                    sess.diagnostics.push(Diagnostic::SubscriptArrayNotArray {
                        array: array.clone(),
                        index: index.clone(),
                        bracket: bracket.clone(),
                    });

                    HErr::err()
                }
            },
        }
    }
}

impl HirCompileFrom<AExpr> for HArg {
    fn compile(ast: AExpr, env: &Env, sess: &mut Sess) -> Self {
        let val: Rc<HVal> = ast.compile(env, sess);

        HArg {
            val: val.clone(),
            ty: val.ty.clone(),
            expr: match val.expr.deref() {
                HValExpr::Var { var, .. } => Rc::new(HArgExpr::Name {
                    name: var.name.clone(),
                }),
                _ => {
                    sess.diagnostics
                        .push(Diagnostic::ArgumentNotVariable { val: val.clone() });
                    HErr::err()
                }
            },
        }
    }
}

impl HirCompileFrom<AExpr> for HVal {
    fn compile(ast: AExpr, env: &Env, sess: &mut Sess) -> Self {
        let expr: Rc<HValExpr> = ast.compile(env, sess);

        HVal {
            ty: expr.ty(sess),
            expr,
        }
    }
}

impl HirCompileFrom<AExpr> for HRangeBound {
    fn compile(ast: AExpr, env: &Env, sess: &mut Sess) -> Self {
        let val: Rc<HVal> = ast.compile(env, sess);

        match val.ty.deref() {
            HValTy::Atom { atom_ty } => {
                if let AtomTy::Natural { .. } = &atom_ty.sem {
                    // OK
                } else {
                    sess.diagnostics.push(Diagnostic::RangeBoundNotNatural {
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
                sess.diagnostics.push(Diagnostic::RangeBoundNotNatural {
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
    fn compile(ast: AExpr, env: &Env, sess: &mut Sess) -> Self {
        let val: Rc<HVal> = ast.compile(env, sess);

        HAtom {
            ty: match val.ty.deref() {
                HValTy::Atom { atom_ty } => atom_ty.clone(),
                HValTy::Err => HErr::err(),
                _ => {
                    sess.diagnostics
                        .push(Diagnostic::AtomNotScalar { val: val.clone() });
                    HErr::err()
                }
            },
            val,
        }
    }
}

impl HirCompileFrom<AExpr> for HValExpr {
    fn compile(ast: AExpr, env: &Env, sess: &mut Sess) -> Self {
        match ast {
            AExpr::Ref { ident } => {
                let ident = ident.compile(&(), sess);
                let var = env.resolve(&ident, sess);

                HValExpr::Var { var, ident }
            }
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => HValExpr::Subscript {
                array: (*array).compile(env, sess),
                index: (*index).compile(env, sess),
                bracket,
            },
        }
    }
}

impl HirCompileFrom<ATy, ()> for HAtomTy {
    fn compile(ast: ATy, _: &(), sess: &mut Sess) -> Self {
        let name: Rc<HName> = ast.ident.compile(&(), sess);

        HAtomTy {
            sem: AtomTy::all()
                .into_iter()
                .find(|k| k.name() == name.ident.to_string())
                .unwrap_or_else(|| {
                    sess.diagnostics.push(Diagnostic::InvalidAtomTy {
                        ident: name.clone(),
                    });
                    AtomTy::Err
                }),
            expr: Rc::new(HAtomTyExpr::Name { name }),
        }
    }
}

impl HirCompileFrom<AIdent, ()> for HName {
    fn compile(ast: AIdent, _: &(), _sess: &mut Sess) -> Self {
        let AIdent { token } = ast;
        HName { ident: token }
    }
}

pub fn compile_hir(ast: ASpec, sess: &mut Sess) -> Result<Rc<HSpec>, ()> {
    let main: Rc<HStep> = ast.main.compile(&Env::main(), sess);

    if sess.diagnostics.iter().any(|d| d.is_critical()) {
        Err(())?
    }

    Ok(Rc::new(HSpec {
        funs: main.funs(),
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
