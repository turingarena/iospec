extern crate genco;

use genco::prelude::*;

use crate::lir::*;
use crate::ty::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CppLang;

impl Lang for CppLang {
    type Config = ();
    type Format = ();
    type Item = ();
}

impl FormatInto<CppLang> for LSpec {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let Self { funs, main } = self;
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

impl FormatInto<CppLang> for LFun {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let Self { name, params, ret } = self;
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

impl FormatInto<CppLang> for LParam {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let Self { name, ty } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl FormatInto<CppLang> for LBlock {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        let Self { stmts } = self;
        quote_in! { *tokens =>
            #(for stmt in stmts join (#<push>) => #stmt)
        }
    }
}

impl FormatInto<CppLang> for LExpr {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LExpr::Var { name } => quote_in! { *tokens =>
                #name
            },
            LExpr::Subscript { array, index } => quote_in! { *tokens =>
                #(*array)[#(*index)]
            },
        }
    }
}

impl FormatInto<CppLang> for AtomTy {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            AtomTy::Boolean => quote_in!(*tokens => bool),
            AtomTy::Natural { size } | AtomTy::Integer { size } => {
                quote_in!(*tokens => int#(size.bits())_t)
            }
            AtomTy::Err => unreachable!(),
        }
    }
}

struct Format(AtomTy);

impl FormatInto<CppLang> for Format {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self.0 {
            AtomTy::Boolean => quote_in!(*tokens => "%d"),
            AtomTy::Natural { size } | AtomTy::Integer { size } => match size {
                BitSize::S8 | BitSize::S16 | BitSize::S32 => quote_in!(*tokens => "%d"),
                BitSize::S64 => quote_in!(*tokens => "%lld"),
            },
            AtomTy::Err => unreachable!(),
        }
    }
}

impl FormatInto<CppLang> for LTy {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LTy::Atom { atom } => quote_in!(*tokens => #atom),
            LTy::Array { item } => quote_in!(*tokens => #(*item)*),
        }
    }
}

impl FormatInto<CppLang> for LStmt {
    fn format_into(self, tokens: &mut Tokens<CppLang>) {
        match self {
            LStmt::Decl { name, ty } => quote_in! { *tokens =>
                #ty #name;
            },
            LStmt::Alloc {
                array,
                item_ty,
                size,
            } => quote_in! { *tokens =>
                #array = new #item_ty[#size];
            },
            LStmt::Write { arg, ty } => quote_in! { *tokens =>
                printf(#(Format(ty)) " ", #arg);
            },
            LStmt::Read { arg, ty } => quote_in! { *tokens =>
                scanf(#(Format(ty)), &#arg);
            },
            LStmt::Call { name, args, ret } => match ret {
                Some(ret) => quote_in! { *tokens =>
                    #ret = #name(#(
                        for a in args join (, ) => #a
                    ));
                },
                None => quote_in! { *tokens =>
                    #name(#(
                        for a in args join (, ) => #a
                    ));
                },
            },
            // TODO: bound type?
            LStmt::For {
                index_name,
                bound,
                body,
            } => {
                let i = &index_name;
                quote_in! { *tokens =>
                    for(int #i = 0; #i < #bound; #i++) {
                        #body
                    }
                }
            }
        }
    }
}

pub fn gen_file(spec: LSpec) -> String {
    let tokens: Tokens<CppLang> = quote!(#spec);
    tokens.to_file_string().unwrap()
}
