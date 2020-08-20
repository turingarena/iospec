extern crate genco;

use genco::prelude::*;

use crate::lir::*;
use crate::ty::*;

fn gen_spec(spec: &LSpec) -> Tokens {
    quote! {
        ##include <cstdio>
        ##include <cstdint>

        #(for f in spec.funs.iter() join (#<line>) => #(gen_fun(f)))

        int main() {
            #(gen_block(&spec.main))
        }
    }
}

fn gen_fun(fun: &LFun) -> Tokens {
    let ret = match &fun.ret {
        Some(ret) => gen_atom_ty(ret),
        None => quote!(void),
    };

    let params = fun.params.iter().map(gen_param);

    let f = &fun.name;

    quote! {
        #(ret) #(f)(#(for p in params join (, ) => #(p)));
    }
}

fn gen_param(param: &LParam) -> Tokens {
    let t = gen_expr_ty(&param.ty);
    let x = &param.name;

    quote!(#(t) #(x))
}

fn gen_block(block: &LBlock) -> Tokens {
    quote! {
        #(for i in block.iter() join (#<push>) => #(gen_inst(i)))
    }
}

fn gen_expr(expr: &LExpr) -> Tokens {
    match expr {
        LExpr::Var { name } => quote!(#(name)),
        LExpr::Subscript { array, index } => {
            let a = gen_expr(array);
            let i = gen_expr(index);

            quote!(#(a)[#(i)])
        }
    }
}

fn gen_scanf_format(ty: &AtomTy) -> Tokens {
    match ty {
        AtomTy::Boolean => quote!("%d"),
        AtomTy::Natural { size } | AtomTy::Integer { size } => match size {
            BitSize::S8 | BitSize::S16 | BitSize::S32 => quote!("%d"),
            BitSize::S64 => quote!("%lld"),
        },
        AtomTy::Err => unreachable!(),
    }
}

fn gen_printf_format(ty: &AtomTy) -> Tokens {
    match ty {
        AtomTy::Boolean => quote!("%d "),
        AtomTy::Natural { size } | AtomTy::Integer { size } => match size {
            BitSize::S8 | BitSize::S16 | BitSize::S32 => quote!("%d "),
            BitSize::S64 => quote!("%lld "),
        },
        AtomTy::Err => unreachable!(),
    }
}

fn gen_atom_ty(ty: &AtomTy) -> Tokens {
    match ty {
        AtomTy::Boolean => quote!(bool),
        AtomTy::Natural { size } | AtomTy::Integer { size } => match size {
            BitSize::S8 => quote!(int8_t),
            BitSize::S16 => quote!(int16_t),
            BitSize::S32 => quote!(int32_t),
            BitSize::S64 => quote!(int64_t),
        },
        AtomTy::Err => unreachable!(),
    }
}

fn gen_expr_ty(ty: &LTy) -> Tokens {
    match ty {
        LTy::Atom { atom } => gen_atom_ty(atom),
        LTy::Array { item } => quote!(#(gen_expr_ty(item))*),
    }
}

fn gen_inst(inst: &LStmt) -> Tokens {
    match inst {
        LStmt::Decl { name, ty } => quote! {
            #(gen_expr_ty(ty)) #(name);
        },
        LStmt::Alloc { array, ty, size } => {
            let a = gen_expr(array);
            let t = gen_expr_ty(ty);
            let n = gen_expr(size);

            quote![
                #(a) = new #(t)[#(n)];
            ]
        }
        LStmt::Write { arg, ty } => quote! {
            printf(#(gen_printf_format(ty)), #(gen_expr(arg)));
        },
        LStmt::Read { arg, ty } => quote! {
            scanf(#(gen_scanf_format(ty)), &#(gen_expr(arg)));
        },
        LStmt::Call {
            name,
            args,
            ret: None,
        } => quote! {
            #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        LStmt::Call {
            name,
            args,
            ret: Some(ret),
        } => quote! {
            #(gen_expr(ret)) = #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        LStmt::For {
            index_name,
            bound,
            body,
        } => {
            let i = index_name;
            let n = gen_expr(bound);

            quote![
                for(int #(i) = 0; #(i) < #(n); #(i)++) {
                    #(gen_block(body))
                }
            ]
        }
    }
}

pub fn gen_file(spec: &LSpec) -> String {
    gen_spec(spec).to_file_string().unwrap()
}
