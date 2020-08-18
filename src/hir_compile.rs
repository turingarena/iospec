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

trait HirCompileFrom<T, E = Env> {
    fn compile(ast: T, env: &E) -> Self;
}

impl<T, U, E> HirCompileFrom<Box<T>, E> for U
where
    U: HirCompileFrom<T, E>,
{
    fn compile(ast: Box<T>, env: &E) -> Self {
        U::compile(*ast, env)
    }
}

trait HirCompileInto<T, E> {
    fn compile(self: Self, env: &E) -> Rc<T>;
}

impl<U, T, E> HirCompileInto<U, E> for T
where
    U: HirCompileFrom<T, E>,
{
    fn compile(self: Self, env: &E) -> Rc<U> {
        Rc::new(U::compile(self, env))
    }
}

impl HStmt {
    fn funs(self: &Self) -> Vec<Rc<HFun>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.funs()).collect(),
            HStmt::Call { fun, .. } => vec![fun.clone()],
            HStmt::For { body, .. } => body.funs(),
            _ => Vec::new(),
        }
    }

    fn vars(self: &Self) -> Vec<Rc<HVar>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.vars()).collect(),
            HStmt::Read { args, .. } => args.iter().map(hir_node_var).map(Rc::new).collect(),
            HStmt::Call { fun, .. } => fun.ret.iter().map(hir_node_var).map(Rc::new).collect(),
            HStmt::For { body, .. } => body.vars(),
            _ => Vec::new(),
        }
    }

    // TODO: make private?
    pub fn allocs(self: &Self) -> Vec<Rc<HDataNode>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.allocs()).collect(),
            HStmt::Read { args, .. } => args.iter().map(|d| d.node.clone()).collect(),
            HStmt::Call { fun, .. } => fun.ret.iter().map(|d| d.node.clone()).collect(),
            HStmt::For { body, .. } => body
                .allocs()
                .into_iter()
                .flat_map(|node| match node.expr.deref() {
                    // TODO: check index somewhere?
                    HDataExpr::Subscript { array, .. } => Some(array.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }
}

impl HirCompileFrom<ABlock> for HStmt {
    fn compile(ast: ABlock, env: &Env) -> Self {
        let mut env = env.clone();
        let mut stmts = Vec::new();

        for stmt in ast.stmts {
            let stmt: Rc<HStmt> = stmt.compile(&env);
            env.refs.extend(stmt.vars().into_iter());
            stmts.push(stmt);
        }

        HStmt::Block { stmts }
    }
}

impl HirCompileFrom<AStmt> for HStmt {
    fn compile(ast: AStmt, env: &Env) -> Self {
        match ast {
            AStmt::Read { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmt::Read {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env)).collect(),
                    arg_commas,
                    semi,
                }
            }
            AStmt::Write { kw, args, semi } => {
                let (args, arg_commas) = unzip_punctuated(args);

                HStmt::Write {
                    kw,
                    args: args.into_iter().map(|a| a.compile(env)).collect(),
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
                    Some((a, r)) => (Some(a), Some(r.compile(env))),
                    None => (None, None),
                };

                HStmt::Call {
                    kw,
                    fun: Rc::new(HFun {
                        name: name.compile(&()),
                        args: args.into_iter().map(|a| a.compile(env)).collect(),
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
                    index: index.compile(&()),
                    upto,
                    bound: bound.compile(env),
                });

                HStmt::For {
                    kw,
                    body: body.compile(&Env {
                        refs: vec![Rc::new(hir_index_var(&range))],
                        outer: Some(Box::new((*env).clone())),
                        loc: Rc::new(HDataLoc::For {
                            range: range.clone(),
                            parent: env.loc.clone(),
                        }),
                    }),
                    range,
                    body_brace,
                }
            }
        }
    }
}

impl HirCompileFrom<ADef> for HDataAtom {
    fn compile(ast: ADef, env: &Env) -> Self {
        let ADef { expr, colon, ty } = ast;
        let ty: Rc<HAtomTy> = ty.compile(&());

        HDataAtom {
            colon,
            node: expr.compile(&HDefEnv {
                env: env.clone(),
                ty: Rc::new(HValTy::Atom {
                    atom_ty: ty.clone(),
                }),
                loc: env.loc.clone(),
            }),
            ty,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HDataNode {
    fn compile(ast: AExpr, env: &HDefEnv) -> Self {
        let expr: Rc<HDataExpr> = ast.compile(env);

        HDataNode {
            ty: env.ty.clone(),
            root: match expr.deref() {
                HDataExpr::Var { var } => var.clone(),
                HDataExpr::Subscript { array, .. } => array.root.clone(),
            },
            expr,
        }
    }
}

impl HirCompileFrom<AExpr, HDefEnv> for HDataExpr {
    fn compile(ast: AExpr, env: &HDefEnv) -> Self {
        match ast {
            AExpr::Ref { ident } => HDataExpr::Var {
                var: ident.compile(env),
            },
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => match env.loc.deref() {
                HDataLoc::For { range, parent } => match *index {
                    AExpr::Ref { ident }
                        if ident.token.to_string() == range.index.token.to_string() =>
                    {
                        let index = Rc::new(HIndex {
                            name: ident.compile(&()),
                            range: range.clone(),
                        });

                        HDataExpr::Subscript {
                            array: array.compile(&HDefEnv {
                                env: env.env.clone(),
                                ty: Rc::new(HValTy::Array {
                                    item: env.ty.clone(),
                                    range: range.clone(),
                                }),
                                loc: parent.clone(),
                            }),
                            bracket,
                            index,
                        }
                    }
                    _ => todo!("recover from invalid expression for index"),
                },
                _ => todo!("recover from invalid expression for index"),
            },
        }
    }
}

impl HirCompileFrom<AIdent, HDefEnv> for HDataVar {
    fn compile(ast: AIdent, env: &HDefEnv) -> Self {
        HDataVar {
            name: ast.compile(&()),
            ty: env.ty.clone(),
        }
    }
}

impl HValExpr {
    fn ty(self: &Self) -> Rc<HValTy> {
        match self {
            HValExpr::Var { var, .. } => var.ty.clone(),
            HValExpr::Subscript { array, .. } => match array.ty.deref() {
                // TODO: check index type as well
                HValTy::Array { item, .. } => item.clone(),
                _ => todo!("recover from invalid array type"),
            },
        }
    }
}

impl HVal {
    fn name(self: &Self) -> Rc<HIdent> {
        match self.expr.deref() {
            HValExpr::Var { var, .. } => var.name.clone(),
            _ => todo!("recover from invalid expr in call args"),
        }
    }
}

impl HirCompileFrom<AExpr> for HArg {
    fn compile(ast: AExpr, env: &Env) -> Self {
        let val: Rc<HVal> = ast.compile(env);

        HArg {
            name: val.name(),
            ty: val.ty.clone(),
            val,
        }
    }
}

impl HirCompileFrom<AExpr> for HVal {
    fn compile(ast: AExpr, env: &Env) -> Self {
        let expr: Rc<HValExpr> = ast.compile(env);

        HVal {
            ty: expr.ty(),
            expr,
        }
    }
}

impl HirCompileFrom<AExpr> for HValExpr {
    fn compile(ast: AExpr, env: &Env) -> Self {
        match ast {
            AExpr::Ref { ident } => {
                let ident = ident.compile(&());
                let var = env
                    .resolve(&ident)
                    .unwrap_or_else(|| todo!("recover from undefined var"));

                HValExpr::Var { var, ident }
            }
            AExpr::Subscript {
                array,
                bracket,
                index,
            } => HValExpr::Subscript {
                array: array.compile(env),
                index: index.compile(env),
                bracket,
            },
        }
    }
}

impl HirCompileFrom<ATy, ()> for HAtomTy {
    fn compile(ast: ATy, _: &()) -> Self {
        HAtomTy {
            ident: ast.ident.compile(&()),
        }
    }
}

impl HirCompileFrom<AIdent, ()> for HIdent {
    fn compile(ast: AIdent, _: &()) -> Self {
        let AIdent { token } = ast;
        HIdent { token }
    }
}

pub fn compile_hir(ast: ASpec) -> HSpec {
    let main: Rc<HStmt> = ast.main.compile(&Env {
        refs: Vec::new(),
        outer: None,
        loc: Rc::new(HDataLoc::Main),
    });

    HSpec {
        funs: main.funs(),
        main,
    }
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

fn hir_node_var(atom: &Rc<HDataAtom>) -> HVar {
    HVar {
        name: atom.node.root.name.clone(),
        ty: atom.node.root.ty.clone(),
        kind: HVarKind::Data {
            var: atom.node.root.clone(),
        },
    }
}

fn hir_index_var(range: &Rc<HRange>) -> HVar {
    HVar {
        name: range.index.clone(),
        ty: Rc::new(HValTy::Index {
            range: range.clone(),
        }),
        kind: HVarKind::Index {
            range: range.clone(),
        },
    }
}
