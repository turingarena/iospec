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
            HValExpr::Err => quote_in!(*tokens => <<invalid val expr>>),
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
