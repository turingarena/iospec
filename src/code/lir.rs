//! Low-level Intermediate Representation (LIR), used for code generation.
//!
//! LIR is meant to be directly translated into parser code.
//! It is a tree without references to higher level nodes (HIR).
//! Thus, it can be easily traversed structurally and translated into code.
//! Moreover, each node owns (a reference to) the configuration of the target language,
//! so that nodes can be translated to target code without contextual information.

use std::ops::Deref;
use std::rc::Rc;

use crate::atom::*;
use crate::spec::rel::RelOp;

pub trait CodeLang: Clone {}

#[derive(Debug, Clone)]
pub struct Lir<L: CodeLang, T> {
    pub lang: L,
    pub code: Rc<T>,
}

impl<L: CodeLang, T> Lir<L, T> {
    pub fn with_lang<FF: CodeLang>(self: &Self, lang: FF) -> Lir<FF, T> {
        Lir {
            code: self.code.clone(),
            lang,
        }
    }
}

impl<L: CodeLang, T> AsRef<T> for Lir<L, T> {
    fn as_ref(self: &Self) -> &T {
        self.code.as_ref()
    }
}

impl<L: CodeLang, T> Deref for Lir<L, T> {
    type Target = T;

    fn deref(self: &Self) -> &Self::Target {
        self.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct LSpec<L: CodeLang> {
    pub funs: Vec<Lir<L, LFun<L>>>,
    pub main: Lir<L, LBlock<L>>,
}

#[derive(Debug, Clone)]
pub struct LFun<L: CodeLang> {
    pub name: String,
    pub params: Vec<Lir<L, LParam<L>>>,
    pub ret: Option<Lir<L, AtomTy>>,
}

#[derive(Debug, Clone)]
pub struct LParam<L: CodeLang> {
    pub name: String,
    pub ty: Lir<L, LTy<L>>,
}

#[derive(Debug, Clone)]
pub struct LBlock<L: CodeLang> {
    pub stmts: Vec<Lir<L, LStmt<L>>>,
}

#[derive(Debug, Clone)]
pub enum LStmt<L: CodeLang> {
    Read {
        args: Vec<Lir<L, LReadArg<L>>>,
    },
    Write {
        args: Vec<Lir<L, LWriteArg<L>>>,
    },
    Call {
        decl: Option<Lir<L, LDecl<L>>>,
        name: String,
        args: Vec<Lir<L, LExpr<L>>>,
        ret: Option<Lir<L, LExpr<L>>>,
    },
    For {
        allocs: Vec<Lir<L, LAlloc<L>>>,
        index: Lir<L, LDecl<L>>,
        index_ty: Lir<L, AtomTy>,
        bound: Lir<L, LExpr<L>>,
        body: Lir<L, LBlock<L>>,
    },
    Assume {
        cond: Lir<L, LExpr<L>>,
    },
}

#[derive(Debug, Clone)]
pub struct LDecl<L: CodeLang> {
    pub ty: Lir<L, LTy<L>>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct LReadArg<L: CodeLang> {
    pub decl: Option<Lir<L, LDecl<L>>>,
    pub expr: Lir<L, LExpr<L>>,
    pub ty: Lir<L, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LWriteArg<L: CodeLang> {
    pub expr: Lir<L, LExpr<L>>,
    pub ty: Lir<L, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LAlloc<L: CodeLang> {
    pub decl: Option<Lir<L, LDecl<L>>>,
    pub array: Lir<L, LExpr<L>>,
    pub item_ty: Lir<L, LTy<L>>,
    pub size: Lir<L, LExpr<L>>,
}

#[derive(Debug, Clone)]
pub enum LExpr<L: CodeLang> {
    Var {
        name: String,
    },
    Subscript {
        array: Box<Lir<L, LExpr<L>>>,
        index: Box<Lir<L, LExpr<L>>>,
    },
    Lit {
        value: i64,
    },
    Paren {
        inner: Box<Lir<L, LExpr<L>>>,
    },
    Mul {
        factors: Vec<Lir<L, LExpr<L>>>,
    },
    Sum {
        terms: Vec<(Lir<L, Option<LSign>>, Lir<L, LExpr<L>>)>,
    },
    Rel {
        left: Box<Lir<L, LExpr<L>>>,
        op: Lir<L, RelOp>,
        right: Box<Lir<L, LExpr<L>>>,
    },
    And {
        clauses: Vec<Lir<L, LExpr<L>>>,
    },
}

#[derive(Debug, Clone)]
pub enum LSign {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum LTy<L: CodeLang> {
    Atom { atom: Lir<L, AtomTy> },
    Array { item: Box<Lir<L, LTy<L>>> },
}
