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

impl<T, U, E> HirCompileFrom<Box<T>, E> for U
where
    U: HirCompileFrom<T, E>,
{
    fn compile(ast: Box<T>, env: &E, sess: &mut Sess) -> Self {
        U::compile(*ast, env, sess)
    }
}

trait HirCompileInto<T, E> {
    fn compile(self: Self, env: &E, sess: &mut Sess) -> Rc<T>;
}

impl<U, T, E> HirCompileInto<U, E> for T
where
    U: HirCompileFrom<T, E>,
{
    fn compile(self: Self, env: &E, sess: &mut Sess) -> Rc<U> {
        Rc::new(U::compile(self, env, sess))
    }
}

impl HStmt {
    fn funs(self: &Self) -> Vec<Rc<HFun>> {
        match self.expr.deref() {
            HStmtExpr::Block { stmts } => stmts.iter().flat_map(|s| s.funs()).collect(),
            HStmtExpr::Call { fun, .. } => vec![fun.clone()],
            HStmtExpr::For { body, .. } => body.funs(),
            _ => Vec::new(),
        }
    }

    fn vars(self: &Self) -> Vec<Rc<HVar>> {
        match self.expr.deref() {
            HStmtExpr::Block { stmts } => stmts.iter().flat_map(|s| s.vars()).collect(),
            HStmtExpr::Read { args, .. } => {
                args.iter().flat_map(hir_node_var).map(Rc::new).collect()
            }
            HStmtExpr::Call { fun, .. } => {
                fun.ret.iter().flat_map(hir_node_var).map(Rc::new).collect()
            }
            HStmtExpr::For { body, .. } => body.vars(),
            _ => Vec::new(),
        }
    }
}

impl HStmtExpr {
    fn allocs(self: &Self) -> Vec<Rc<HDataNode>> {
        match self {
            HStmtExpr::Block { stmts } => stmts
                .iter()
                .flat_map(|s| s.allocs.iter())
                .cloned()
                .collect(),
            HStmtExpr::Read { args, .. } => args.iter().map(|d| d.node.clone()).collect(),
            HStmtExpr::Call { fun, .. } => fun.ret.iter().map(|d| d.node.clone()).collect(),
            HStmtExpr::For { body, .. } => body
                .allocs
                .iter()
                .flat_map(|node| match node.expr.deref() {
                    HDataExpr::Subscript { array, .. } => Some(array.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }
}

impl HirCompileFrom<ABlock> for HStmt {
    fn compile(ast: ABlock, env: &Env, sess: &mut Sess) -> Self {
        let expr: Rc<HStmtExpr> = ast.compile(env, sess);
        HStmt {
            allocs: expr.allocs(),
            expr,
        }
    }
}

impl HirCompileFrom<AStmt> for HStmt {
    // TODO: manage to deduplicate
    fn compile(ast: AStmt, env: &Env, sess: &mut Sess) -> Self {
        let expr: Rc<HStmtExpr> = ast.compile(env, sess);
        HStmt {
            allocs: expr.allocs(),
            expr,
        }
    }
}

impl HirCompileFrom<ABlock> for HStmtExpr {
    fn compile(ast: ABlock, env: &Env, sess: &mut Sess) -> Self {
        let mut env = env.clone();
        let mut stmts = Vec::new();

        for stmt in ast.stmts {
            let stmt: Rc<HStmt> = stmt.compile(&env, sess);
            for var in stmt.vars() {
                env.declare(&var, sess);
            }
            stmts.push(stmt);
        }

        HStmtExpr::Block { stmts }
    }
}

impl HirCompileFrom<AStmt> for HStmtExpr {
    fn compile(ast: AStmt, env: &Env, sess: &mut Sess) -> Self {
        match ast {
            AStmt::Read { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmtExpr::Read {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env, sess)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Write { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmtExpr::Write {
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

                HStmtExpr::Call {
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
                // TODO: check bound is a natural number
                let range = Rc::new(HRange {
                    index: index.compile(&(), sess),
                    upto,
                    bound: bound.compile(env, sess),
                });

                HStmtExpr::For {
                    kw,
                    body: body.compile(
                        &Env {
                            refs: vec![Rc::new(hir_index_var(&range))],
                            outer: Some(Box::new((*env).clone())),
                            loc: Rc::new(HDataLoc::For {
                                range: range.clone(),
                                parent: env.loc.clone(),
                            }),
                        },
                        sess,
                    ),
                    range,
                    body_brace,
                }
            }
        }
    }
}

impl HirCompileFrom<ADef> for HDataAtom {
    fn compile(ast: ADef, env: &Env, sess: &mut Sess) -> Self {
        let ADef { expr, colon, ty } = ast;
        let ty: Rc<HAtomTy> = ty.compile(&(), sess);

        HDataAtom {
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

impl HirCompileFrom<AExpr, HDefEnv> for HDataNode {
    fn compile(ast: AExpr, env: &HDefEnv, sess: &mut Sess) -> Self {
        let expr: Rc<HDataExpr> = ast.compile(env, sess);

        HDataNode {
            ty: env.ty.clone(),
            root_var: match expr.deref() {
                HDataExpr::Var { var } => var.clone(),
                HDataExpr::Subscript { array, .. } => array.root_var.clone(),
                HDataExpr::Err => HErr::err(),
            },
            var: match expr.deref() {
                HDataExpr::Var { var } => Some(var.clone()),
                _ => None,
            },
            expr,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HDataExpr {
    fn compile(ast: AExpr, env: &HDefEnv, sess: &mut Sess) -> Self {
        match ast {
            AExpr::Ref { ident } => HDataExpr::Var {
                var: ident.compile(env, sess),
            },
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => match env.loc.deref() {
                HDataLoc::For { range, parent } => match *index {
                    AExpr::Ref { ident } => {
                        let name: Rc<HIdent> = ident.compile(&(), sess);

                        if name.to_string() == range.index.to_string() {
                            let index = Rc::new(HIndex {
                                name: name.clone(),
                                range: range.clone(),
                            });

                            HDataExpr::Subscript {
                                array: array.compile(
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
                            HDataExpr::Err
                        }
                    }
                    _ => {
                        sess.diagnostics
                            .push(Diagnostic::SubscriptDefIndexNotMatched {
                                range: Some(range.clone()),
                                bracket: bracket.clone(),
                                name: None,
                            });
                        HDataExpr::Err
                    }
                },
                _ => {
                    sess.diagnostics
                        .push(Diagnostic::SubscriptDefIndexNotMatched {
                            range: None,
                            bracket: bracket.clone(),
                            name: None,
                        });
                    HDataExpr::Err
                }
            },
        }
    }
}

impl HirCompileFrom<AIdent, HDefEnv> for HDataVar {
    fn compile(ast: AIdent, env: &HDefEnv, sess: &mut Sess) -> Self {
        HDataVar {
            expr: Rc::new(HDataVarExpr::Name {
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
                array: array.compile(env, sess),
                index: index.compile(env, sess),
                bracket,
            },
        }
    }
}

impl HirCompileFrom<ATy, ()> for HAtomTy {
    fn compile(ast: ATy, _: &(), sess: &mut Sess) -> Self {
        let ident: Rc<HIdent> = ast.ident.compile(&(), sess);

        HAtomTy {
            sem: AtomTy::all()
                .into_iter()
                .find(|k| k.name() == ident.token.to_string())
                .unwrap_or_else(|| {
                    sess.diagnostics.push(Diagnostic::InvalidAtomTy {
                        ident: ident.clone(),
                    });
                    AtomTy::Err
                }),
            expr: Rc::new(HAtomTyExpr::Ident { ident }),
        }
    }
}

impl HirCompileFrom<AIdent, ()> for HIdent {
    fn compile(ast: AIdent, _: &(), _sess: &mut Sess) -> Self {
        let AIdent { token } = ast;
        HIdent { token }
    }
}

pub fn compile_hir(ast: ASpec, sess: &mut Sess) -> Result<Rc<HSpec>, ()> {
    let main: Rc<HStmt> = ast.main.compile(
        &Env {
            refs: Vec::new(),
            outer: None,
            loc: Rc::new(HDataLoc::Main),
        },
        sess,
    );

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

fn hir_node_var(atom: &Rc<HDataAtom>) -> Option<HVar> {
    if let HDataVarExpr::Name { name } = atom.node.root_var.expr.deref() {
        Some(HVar {
            name: name.clone(),
            ty: atom.node.root_var.ty.clone(),
            kind: Rc::new(HVarKind::Data {
                var: atom.node.root_var.clone(),
            }),
        })
    } else {
        None
    }
}

fn hir_index_var(range: &Rc<HRange>) -> HVar {
    HVar {
        name: range.index.clone(),
        ty: range.bound.val.ty.clone(),
        kind: Rc::new(HVarKind::Index {
            range: range.clone(),
        }),
    }
}
