use genco::prelude::*;

use crate::spec::hir::*;

impl FormatInto<()> for &HAtomDef {
    fn format_into(self: Self, tokens: &mut Tokens) {
        quote_in!(*tokens => #(self.node.as_ref()))
    }
}

impl FormatInto<()> for &HNodeDef {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match &self.expr {
            HNodeDefExpr::Var { var } => quote_in!(*tokens => #(var.as_ref())),
            HNodeDefExpr::Subscript { array, index, .. } => {
                quote_in!(*tokens => #(array.as_ref())[#(index.as_ref())])
            }
            HNodeDefExpr::Err => quote_in!(*tokens => <<invalid def expr>>),
        }
    }
}

impl FormatInto<()> for &HVarDef {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match &self.expr {
            HVarDefExpr::Name { name } => quote_in!(*tokens => #(name.as_ref())),
            HVarDefExpr::Err => quote_in!(*tokens => <<invalid var>>),
        }
    }
}

impl FormatInto<()> for &HVal {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match &self.expr {
            HValExpr::Var { name, .. } => quote_in!(*tokens => #(name.as_ref())),
            HValExpr::Subscript { array, index, .. } => {
                quote_in!(*tokens => #(array.as_ref())[#(index.as_ref())])
            }
            HValExpr::Lit { token, .. } => quote_in!(*tokens => #(token.to_string())),
            HValExpr::Paren { inner, .. } => quote_in!(*tokens => (#(inner.as_ref()))),
            HValExpr::Err => quote_in!(*tokens => <<invalid val expr>>),
            HValExpr::Mul { factors, .. } => quote_in!(*tokens =>
                #(for f in factors join ( * ) => #(f.as_ref()))
            ),
            HValExpr::Sum { terms, .. } => quote_in!(*tokens =>
                #(for (sign, term) in terms join ( ) => #sign#(term.as_ref()))
            ),
            HValExpr::RelChain { rels } => quote_in!(*tokens =>
                #(
                    for (left, op, right) in rels join ( and ) =>
                    #(left.val.as_ref()) #(format!("{}", op)) #(right.val.as_ref())
                )
            ),
        }
    }
}

impl FormatInto<()> for &HSign {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self {
            HSign::Plus(None) => quote_in!(*tokens => ),
            HSign::Plus(Some(_)) => quote_in!(*tokens => +),
            HSign::Minus(_) => quote_in!(*tokens => -),
        }
    }
}

impl FormatInto<()> for &HAtom {
    fn format_into(self: Self, tokens: &mut Tokens) {
        quote_in!(*tokens => #(self.val.as_ref()))
    }
}

impl FormatInto<()> for &HName {
    fn format_into(self: Self, tokens: &mut Tokens) {
        quote_in!(*tokens => #(self.ident.to_string()))
    }
}

impl FormatInto<()> for &HAtomTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match &self.sem {
            Some(sem) => quote_in!(*tokens => #(sem.name())),
            None => quote_in!(*tokens => <<invalid atom type>>),
        }
    }
}

impl FormatInto<()> for &HValTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self {
            HValTy::Atom { atom_ty } => quote_in!(*tokens => #(atom_ty.as_ref())),
            HValTy::Array { item, .. } => quote_in!(*tokens => #(item.as_ref())[]),
            HValTy::Err => {}
        }
    }
}

pub fn quote_hir<'a, T>(hir: &'a T) -> String
where
    &'a T: FormatInto<()>,
{
    let tokens = quote!(#hir);
    tokens.to_string().unwrap()
}
