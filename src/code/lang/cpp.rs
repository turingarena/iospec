extern crate genco;

use genco::prelude::*;

use crate::atom::*;
use crate::code::lang::common::CommonLang;
use crate::spec::rel::RelOp;

use super::lir::*;
use super::lir_code::*;

#[derive(Debug, Clone)]
pub struct Cpp;

impl CodeLang for Cpp {}

impl Code<Cpp> for LSpec<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        let LSpec { funs, main } = self;
        quote_in! {*tokens =>
            ##include <cstdio>
            ##include <cstdint>
            ##include <cassert>

            #(
                for f in funs join (#<line>) =>
                #f
            )

            int main() {
                #main
            }
        }
    }
}

impl Code<Cpp> for LFun<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        let LFun { name, params, ret } = self;
        quote_in! {*tokens =>
            #(match ret {
                Some(ret) => #ret,
                None => void,
            }) #name(#(
                for p in params join (, ) =>
                #p
            ));
        }
    }
}

impl Code<Cpp> for LParam<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        let LParam { name, ty } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl Code<Cpp> for LBlock<Cpp> {
    fn code_into(self: &Self, lang: &Cpp, tokens: &mut Tokens) {
        CommonLang(self).code_into(lang, tokens)
    }
}

impl Code<Cpp> for LExpr<Cpp> {
    fn code_into(self: &Self, lang: &Cpp, tokens: &mut Tokens) {
        CommonLang(self).code_into(lang, tokens)
    }
}

impl Code<Cpp> for LSign {
    fn code_into(self: &Self, lang: &Cpp, tokens: &mut Tokens) {
        CommonLang(self).code_into(lang, tokens)
    }
}

impl Code<Cpp> for RelOp {
    fn code_into(self: &Self, lang: &Cpp, tokens: &mut Tokens) {
        CommonLang(self).code_into(lang, tokens)
    }
}

impl Code<Cpp> for AtomTy {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        match self {
            AtomTy::Bool => quote_in!(*tokens => bool),
            AtomTy::Nat { size } | AtomTy::Int { size } => {
                quote_in!(*tokens => int#(size.bits())_t)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct CppStdioFormat;

impl CodeLang for CppStdioFormat {}

impl Code<CppStdioFormat> for AtomTy {
    fn code_into(self: &Self, _lang: &CppStdioFormat, tokens: &mut Tokens) {
        match self {
            AtomTy::Bool => quote_in!(*tokens => %d),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 | BitSize::S16 | BitSize::S32 => quote_in!(*tokens => %d),
                BitSize::S64 => quote_in!(*tokens => %lld),
            },
        }
    }
}

impl Code<Cpp> for LTy<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        match self {
            LTy::Atom { atom } => quote_in!(*tokens => #atom),
            LTy::Array { item } => quote_in!(*tokens => #item*),
        }
    }
}

impl Code<Cpp> for LStmt<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        match self {
            LStmt::Write { args } => quote_in! { *tokens =>
                printf(#(quoted(quote!(
                    #(
                        for arg in args join ( ) =>
                        #(&arg.ty.with_lang(CppStdioFormat))
                    )#(r"\n")
                ))), #(
                    for arg in args join (, ) =>
                    #(&arg.expr)
                ));
            },
            LStmt::Read { args } => quote_in! { *tokens =>
                #(
                    for arg in args join (#<push>) =>
                    #(match arg.decl.as_ref() {
                        Some(decl) => #decl;,
                        None => ,
                    })
                )
                scanf(#(quoted(quote!(
                    #(
                        for arg in args join () =>
                        #(&arg.ty.with_lang(CppStdioFormat))
                    )
                ))), #(
                    for arg in args join (, ) =>
                    &#(&arg.expr)
                ));
            },
            LStmt::Call {
                decl,
                name,
                args,
                ret,
            } => match ret {
                Some(ret) => quote_in! { *tokens =>
                    #(match decl {
                        Some(decl) => #decl,
                        None => #ret,
                    }) = #name(#(
                        for a in args join (, ) => #a
                    ));
                },
                None => quote_in! { *tokens =>
                    #name(#(
                        for a in args join (, ) => #a
                    ));
                },
            },
            LStmt::For {
                allocs,
                index_name: i,
                index_ty,
                bound,
                body,
                ..
            } => quote_in! { *tokens =>
                #(
                    for alloc in allocs join (#<push>) =>
                    #alloc
                )
                for(#index_ty #i = 0; #i < #bound; #i++) {
                    #body
                }
            },
            LStmt::Assume { cond } => quote_in! { *tokens =>
                assert(#cond);
            },
        }
    }
}

impl Code<Cpp> for LDecl<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        let LDecl { ty, name } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl Code<Cpp> for LAlloc<Cpp> {
    fn code_into(self: &Self, _lang: &Cpp, tokens: &mut Tokens) {
        let LAlloc {
            decl,
            array,
            item_ty,
            size,
        } = self;
        quote_in! { *tokens =>
            #(match decl {
                Some(decl) => #decl,
                None => #array,
            }) = new #item_ty[#size];
        }
    }
}
