extern crate genco;

use genco::prelude::*;

use crate::atom::*;
use crate::spec::rel::RelOp;

use super::lir::*;
use super::lir_code::*;

#[derive(Debug, Clone)]
pub struct Cpp;

impl LirConfig for Cpp {}

impl Code for LSpec<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
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

impl Code for LFun<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
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

impl Code for LParam<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        let LParam { name, ty } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl Code for LBlock<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        let LBlock { stmts } = self;
        quote_in! { *tokens =>
            #(for stmt in stmts join (#<line>) => #stmt)
        }
    }
}

impl Code for LExpr<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self {
            LExpr::Var { name } => quote_in! { *tokens =>
                #name
            },
            LExpr::Subscript { array, index } => quote_in! { *tokens =>
                #array[#index]
            },
            LExpr::Lit { value } => quote_in! { *tokens =>
                #(*value)
            },
            LExpr::Paren { inner } => quote_in! { *tokens =>
                (#inner)
            },
            LExpr::Mul { factors } => quote_in! { *tokens =>
                #(for f in factors join ( * ) => #f)
            },
            LExpr::Sum { terms } => quote_in! { *tokens =>
                #(for (sign, t) in terms join ( ) => #sign #t)
            },
            LExpr::Rel { left, op, right } => quote_in! { *tokens =>
                #left #op #right
            },
            LExpr::And { clauses } => quote_in! { *tokens =>
                #(for clause in clauses join ( && ) => #clause)
            },
        }
    }
}

impl Code for Option<LSign<Cpp>> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self {
            Some(LSign::Plus(_)) => quote_in!(*tokens => +),
            Some(LSign::Minus(_)) => quote_in!(*tokens => -),
            None => (),
        }
    }
}

impl Code for Lir<Cpp, RelOp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        quote_in!(*tokens => #(self.to_string()))
    }
}

impl Code for Lir<Cpp, AtomTy> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self.as_ref() {
            AtomTy::Bool => quote_in!(*tokens => bool),
            AtomTy::Nat { size } | AtomTy::Int { size } => {
                quote_in!(*tokens => int#(size.bits())_t)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct CppFormat;

impl LirConfig for CppFormat {}

impl Code for Lir<CppFormat, AtomTy> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self.as_ref() {
            AtomTy::Bool => quote_in!(*tokens => %d),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 | BitSize::S16 | BitSize::S32 => quote_in!(*tokens => %d),
                BitSize::S64 => quote_in!(*tokens => %lld),
            },
        }
    }
}

impl Code for LTy<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self {
            LTy::Atom { atom } => quote_in!(*tokens => #atom),
            LTy::Array { item } => quote_in!(*tokens => #item*),
        }
    }
}

impl Code for LStmt<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        match self {
            LStmt::Write { args } => quote_in! { *tokens =>
                printf(#(quoted(quote!(
                    #(
                        for arg in args join ( ) =>
                        #(&arg.ty.with_config(CppFormat))
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
                        #(&arg.ty.with_config(CppFormat))
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
                index,
                index_ty,
                bound,
                body,
            } => {
                let i = &index.name;
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
            LStmt::Assume { cond } => quote_in! { *tokens =>
                assert(#cond);
            },
        }
    }
}

impl Code for LDecl<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        let LDecl { ty, name } = self;
        quote_in! { *tokens =>
            #ty #name
        }
    }
}

impl Code for LAlloc<Cpp> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
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
