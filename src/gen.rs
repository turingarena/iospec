extern crate genco;

use genco::prelude::*;

use crate::mir::*;

fn gen_spec(spec: &MirSpec) -> Tokens {
    quote! {
        int main() {
            #(gen_block(&spec.main))
        }
    }
}

fn gen_block(block: &MirBlock) -> Tokens {
    quote! {
        #(for i in block.iter() join (#<push>) => #(gen_inst(i)))
    }
}

fn gen_expr(expr: &MirExpr) -> Tokens {
    match expr {
        MirExpr::Var { name } => {
            quote!(#(name))
        }
        MirExpr::Subscript { array, index } => {
            let a = gen_expr(array);
            let i = gen_expr(index);

            quote!(#(a)[#(i)])
        }
    }
}

fn gen_scanf_format(ty: &MirDefTy) -> Tokens {
    match ty {
        MirDefTy::I32 | MirDefTy::N32 => quote!("%d"),
        MirDefTy::I64 | MirDefTy::N64 => quote!("%lld"),
    }
}

fn gen_printf_format(ty: &MirDefTy) -> Tokens {
    match ty {
        MirDefTy::I32 | MirDefTy::N32 => quote!("%d "),
        MirDefTy::I64 | MirDefTy::N64 => quote!("%lld "),
    }
}

fn gen_left_type(ty: &MirConsTy) -> Tokens {
    match ty {
        MirConsTy::Scalar { def } => match def {
            MirDefTy::I32 | MirDefTy::N32 => quote!(int),
            MirDefTy::I64 | MirDefTy::N64 => quote!(int64_t),
        },
        MirConsTy::Array { item } => gen_left_type(item),
    }
}

fn gen_inst(inst: &MirInst) -> Tokens {
    match inst {
        MirInst::Decl { name, ty } => quote! {
            // TODO: array types
            #(gen_left_type(ty)) #(name);
        },
        MirInst::Write { arg, ty } => quote! {
            printf(#(gen_printf_format(ty)), #(gen_expr(arg)));
        },
        MirInst::Read { arg, ty } => quote! {
            scanf(#(gen_scanf_format(ty)), &#(gen_expr(arg)));
        },
        MirInst::Call { name, args, ret: None } => quote! {
            #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        MirInst::Call { name, args, ret: Some(ret) } => quote! {
            #(gen_expr(ret)) = #(name)(#(
                for a in args join (, ) => #(gen_expr(a))
            ));
        },
        MirInst::For { index_name, bound, body } => {
            let i = index_name;
            let n = gen_expr(bound);

            quote! [
                for(int #(i) = 0; #(i) < #(n); #(i)++) {
                    #(gen_block(body))
                }
            ]
        }
        _ => unreachable!(),
    }
}

pub fn gen_file(spec: &MirSpec) -> String {
    gen_spec(spec).to_file_string().unwrap()
}
