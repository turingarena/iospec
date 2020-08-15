extern crate genco;

use genco::prelude::*;

use crate::mir::*;

fn gen_spec(spec: &MSpec) -> Tokens {
    quote! {
        ##include <cstdio>

        #(for f in spec.funs.iter() join (#<line>) => #(gen_fun(f)))

        int main() {
            #(gen_block(&spec.main))
        }
    }
}

fn gen_fun(fun: &MFun) -> Tokens {
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

fn gen_param(param: &MParam) -> Tokens {
    let t = gen_expr_ty(&param.ty);
    let x = &param.name;

    quote!(#(t) #(x))
}

fn gen_block(block: &MBlock) -> Tokens {
    quote! {
        #(for i in block.iter() join (#<push>) => #(gen_inst(i)))
    }
}

fn gen_expr(expr: &MExpr) -> Tokens {
    match expr {
        MExpr::Var { name } => quote!(#(name)),
        MExpr::Subscript { array, index } => {
            let a = gen_expr(array);
            let i = gen_expr(index);

            quote!(#(a)[#(i)])
        }
    }
}

fn gen_scanf_format(ty: &MAtomTy) -> Tokens {
    match ty {
        MAtomTy::I32 | MAtomTy::N32 => quote!("%d"),
        MAtomTy::I64 | MAtomTy::N64 => quote!("%lld"),
    }
}

fn gen_printf_format(ty: &MAtomTy) -> Tokens {
    match ty {
        MAtomTy::I32 | MAtomTy::N32 => quote!("%d "),
        MAtomTy::I64 | MAtomTy::N64 => quote!("%lld "),
    }
}

fn gen_atom_ty(ty: &MAtomTy) -> Tokens {
    match ty {
        MAtomTy::I32 | MAtomTy::N32 => quote!(int),
        MAtomTy::I64 | MAtomTy::N64 => quote!(int64_t),
    }
}

fn gen_expr_ty(ty: &MConsTy) -> Tokens {
    match ty {
        MConsTy::Atom { atom } => gen_atom_ty(atom),
        MConsTy::Array { item } => quote!(#(gen_expr_ty(item))*),
    }
}

fn gen_inst(inst: &MInst) -> Tokens {
    match inst {
        MInst::Decl { name, ty } => quote! {
            #(gen_expr_ty(ty)) #(name);
        },
        MInst::Alloc { array, ty, size } => {
            let a = gen_expr(array);
            let t = gen_expr_ty(ty);
            let n = gen_expr(size);

            quote![
                #(a) = new #(t)[#(n)];
            ]
        }
        MInst::Write { arg, ty } => quote! {
            printf(#(gen_printf_format(ty)), #(gen_expr(arg)));
        },
        MInst::Read { arg, ty } => quote! {
            scanf(#(gen_scanf_format(ty)), &#(gen_expr(arg)));
        },
        MInst::Call {
            name,
            args,
            ret: None,
        } => quote! {
            #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        MInst::Call {
            name,
            args,
            ret: Some(ret),
        } => quote! {
            #(gen_expr(ret)) = #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        MInst::For {
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
        _ => unreachable!(),
    }
}

pub fn gen_file(spec: &MSpec) -> String {
    gen_spec(spec).to_file_string().unwrap()
}
