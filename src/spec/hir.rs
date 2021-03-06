//! High-level Intermediate Representation (HIR), used for semantic analysis.
//!
//! The HIR has a topology similar to the AST, but it also has links from each node
//! to any other nodes it refers to.
//! E.g., names are resolved by introducing a link to the node where the name is defined.
//!
//! HIR nodes only link to nodes which occur *before* in a post-order traversal of the AST.
//! Hence, HIR nodes result in a directed acyclic graph (DAG).
//! To represent links, nodes are wrapped in `std::rc:Rc` pointers.
//! Since there are no cycles, no `std::rc:Weak` reference is needed.
//!
//! The HIR contains references to all the *tokens* in the original AST, and all the information
//! needed to reconstruct the AST tree, but does not keep any reference to the tree itself.

extern crate proc_macro2;
extern crate syn;

pub use std::rc::Rc;

use crate::atom::*;
use crate::spec::kw;
use crate::spec::rel::*;

pub struct HSpec {
    pub main: Rc<HStep>,
}

/// An executable part of the spec, i.e., either a statement or block (analysis).
pub struct HStep {
    pub expr: HStepExpr,
    /// Data nodes defined inside this statement/block
    pub nodes: Vec<Rc<HNodeDef>>,
    /// Functions called inside this statement/block
    pub funs: Vec<Rc<HFun>>,
}

/// An executable part of the spec, i.e., either a statement or block (construction).
pub enum HStepExpr {
    Seq {
        steps: Vec<Rc<HStep>>,
    },
    Write {
        kw: kw::write,
        args: Vec<Rc<HAtom>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        kw: kw::read,
        args: Vec<Rc<HAtomDef>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        kw: kw::call,
        fun: Rc<HFun>,
        semi: syn::Token![;],
    },
    For {
        kw: syn::Token![for],
        range: Rc<HRange>,
        body_brace: syn::token::Brace,
        body: Rc<HStep>,
    },
    Assume {
        kw: kw::assume,
        cond: Rc<HAtom>,
        semi: syn::Token![;],
    },
}

/// A called function.
/// E.g., `f(A, B) -> C: i32` in `... call f(A, B) -> C: i32; ...`
pub struct HFun {
    pub name: Rc<HName>,
    pub args: Vec<Rc<HArg>>,
    pub ret: Option<Rc<HAtomDef>>,

    pub args_paren: syn::token::Paren,
    pub arg_commas: Vec<syn::Token![,]>,
    pub ret_rarrow: Option<syn::Token![->]>,
}

/// An argument of a called function (analysis).
/// E.g., `A` in `... call f(A, B) -> C: i32; ...`.
pub struct HArg {
    pub expr: HArgExpr,
    pub val: Rc<HVal>,
}

/// An argument of a called function (construction).
/// E.g., `A` in `... call f(A, B) -> C: i32; ...`.
pub enum HArgExpr {
    Name { name: Rc<HName> },
    Err,
}

/// Definition of an atomic value in input/output data.
/// E.g., `A[i][j]: n32` in `... read A[i][j]: n32; ...`.
pub struct HAtomDef {
    pub node: Rc<HNodeDef>,
    pub colon: syn::Token![:],
    pub ty: Rc<HAtomTy>,
}

/// Definition of a value (either atomic or aggregate) in input/output data (analysis).
/// E.g., `A`, `A[i]` and `A[i][j]` in `... read A[i][j]: n32; ...`.
pub struct HNodeDef {
    pub expr: HNodeDefExpr,
    pub ty: Rc<HValTy>,
    pub root_var: Rc<HVarDef>,
}

/// Definition of a value (either atomic or aggregate) in input/output data (construction).
/// E.g., `A`, `A[i]` and `A[i][j]` in `... read A[i][j]: n32; ...`.
pub enum HNodeDefExpr {
    Var {
        var: Rc<HVarDef>,
    },
    Subscript {
        array: Rc<HNodeDef>,
        bracket: syn::token::Bracket,
        index: Rc<HVal>,
    },
    Err,
}

/// Definition of a variable containing input/output data (analysis).
/// E.g., `A` in `... read A[i][j]: n32; ...`.
pub struct HVarDef {
    pub expr: HVarDefExpr,
    pub ty: Rc<HValTy>,
}

/// Definition of a variable containing input/output data (construction).
/// E.g., `A` in `... read A[i][j]: n32; ...`.
pub enum HVarDefExpr {
    Name { name: Rc<HName> },
    Err,
}

/// Range in a `for` statement.
/// E.g., `i to A[B[i]][k]` in `... for i upto A[B[j]][k] { ... } ...`.
pub struct HRange {
    pub index: Rc<HName>,
    pub upto: kw::upto,
    pub bound: Rc<HRangeBound>,
}

/// Upper bound of a `for ... upto`.
/// E.g., `A[B[i]][k]` in `... for i upto A[B[j]][k] { ... } ...`.
pub struct HRangeBound {
    pub val: Rc<HVal>,
    pub ty: Rc<HAtomTy>,
}

/// Reference to an atomic value of input/output data.
/// E.g., `A[B[i]][k]` in `... write A[B[j]][k]; ...`.
pub struct HAtom {
    pub val: Rc<HVal>,
    pub ty: Rc<HAtomTy>,
}

/// A value (rvalue, atomic or aggregate) defined by an expression (analysis).
/// E.g., `A[B[i]]` in `... for i upto A[B[j]][k] { ... } ...`.
pub struct HVal {
    pub expr: HValExpr,
    pub ty: Rc<HValTy>,
}

/// A value (rvalue, atomic or aggregate) defined by an expression (construction).
/// E.g., `A[B[i]]` in `... for i upto A[B[j]][k] { ... } ...`.
pub enum HValExpr {
    Lit {
        token: syn::LitInt,
        value: Atom,
        ty: Rc<HAtomTy>,
    },
    Var {
        var: Rc<HVar>,
        name: Rc<HName>,
    },
    Subscript {
        array: Rc<HVal>,
        bracket: syn::token::Bracket,
        index: Rc<HVal>,
    },
    Paren {
        paren: syn::token::Paren,
        inner: Rc<HVal>,
    },
    Mul {
        factors: Vec<Rc<HAtom>>,
        ops: Vec<syn::Token![*]>,
        ty: Rc<HAtomTy>,
    },
    Sum {
        terms: Vec<(HSign, Rc<HAtom>)>,
        ty: Rc<HAtomTy>,
    },
    RelChain {
        rels: Vec<HRel>,
    },
    Err,
}

pub type HRel = (Rc<HAtom>, RelOp, Rc<HAtom>);

/// Type of a value (either atomic or aggregate)
pub enum HSign {
    Plus(Option<syn::Token![+]>),
    Minus(syn::Token![-]),
}

/// Type of a value (either atomic or aggregate)
pub enum HValTy {
    Atom { atom_ty: Rc<HAtomTy> },
    Array { item: Rc<HValTy>, range: Rc<HRange> },
    Err,
}

/// Type of an atomic value (analysis)
pub struct HAtomTy {
    pub expr: HAtomTyExpr,
    pub sem: Option<AtomTy>,
}

/// Type of an atomic value (construction)
pub enum HAtomTyExpr {
    Name {
        name: Rc<HName>,
    },
    Lit {
        token: syn::LitInt,
    },
    /// Result of a comparison
    Rel {
        rels: Vec<(Rc<HAtom>, RelOp, Rc<HAtom>)>,
    },
    Err,
}

/// Binding of name to a data variable or an index (analysis).
pub struct HVar {
    pub name: Rc<HName>,
    pub ty: Rc<HValTy>,
    pub expr: HVarExpr,
}

/// Binding of name to a data variable or an index (construction).
pub enum HVarExpr {
    Data { def: Rc<HVarDef> },
    Index { range: Rc<HRange> },
    Err,
}

/// An identifier (in any context)
pub struct HName {
    pub ident: proc_macro2::Ident,
}

impl ToString for HName {
    fn to_string(self: &Self) -> String {
        self.ident.to_string()
    }
}
