extern crate genco;

use genco::prelude::*;

use crate::atom::*;
use crate::code::lir::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CppLang;

impl Lang for CppLang {
    type Config = ();
    type Format = ();
    type Item = ();
}

impl FormatInto<CppLang> for &LSpec {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let LSpec { funs, main } = self;
        quote_in! {*tokens =>
            ##include <cstdio>
            ##include <cstdint>

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

impl FormatInto<CppLang> for &LFun {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
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

impl FormatInto<CppLang> for &LParam {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let LParam { name, ty } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl FormatInto<CppLang> for &LBlock {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let LBlock { stmts } = self;
        quote_in! { *tokens =>
            #(for stmt in stmts join (#<line>) => #stmt)
        }
    }
}

impl FormatInto<CppLang> for &LExpr {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LExpr::Var { name } => quote_in! { *tokens =>
                #name
            },
            LExpr::Subscript { array, index } => quote_in! { *tokens =>
                #(array.as_ref())[#(index.as_ref())]
            },
            LExpr::Lit { value } => quote_in! { *tokens =>
                #(*value)
            },
            LExpr::Paren { inner } => quote_in! { *tokens =>
                (#(inner.as_ref()))
            },
            LExpr::Mul { factors } => quote_in! { *tokens =>
                #(for f in factors join ( * ) => #f)
            },
        }
    }
}

impl FormatInto<CppLang> for &AtomTy {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            AtomTy::Bool => quote_in!(*tokens => bool),
            AtomTy::Nat { size } | AtomTy::Int { size } => {
                quote_in!(*tokens => int#(size.bits())_t)
            }
        }
    }
}

struct Format<'a>(&'a AtomTy);

impl FormatInto<CppLang> for Format<'_> {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self.0 {
            AtomTy::Bool => quote_in!(*tokens => %d),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 | BitSize::S16 | BitSize::S32 => quote_in!(*tokens => %d),
                BitSize::S64 => quote_in!(*tokens => %lld),
            },
        }
    }
}

impl FormatInto<CppLang> for &LTy {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LTy::Atom { atom } => quote_in!(*tokens => #atom),
            LTy::Array { item } => quote_in!(*tokens => #(item.as_ref())*),
        }
    }
}

impl FormatInto<CppLang> for &LStmt {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LStmt::Write { args } => quote_in! { *tokens =>
                printf(#(quoted(quote!(
                    #(
                        for arg in args join ( ) =>
                        #(Format(&arg.ty))
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
                        #(Format(&arg.ty))
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
                index: LDecl { name: i, .. },
                index_ty,
                bound,
                body,
            } => {
                quote_in! { *tokens =>
                    #(
                        for alloc in allocs join (#<push>) =>
                        #alloc
                    )
                    for(#index_ty #i = 0; #i < #bound; #i++) {
                        #body
                    }
                }
            }
        }
    }
}

impl FormatInto<CppLang> for &LDecl {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let LDecl { ty, name } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl FormatInto<CppLang> for &LAlloc {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
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

pub fn gen_file(spec: &LSpec) -> String {
    let tokens: Tokens<CppLang> = quote!(#spec);
    tokens.to_file_string().unwrap()
}
