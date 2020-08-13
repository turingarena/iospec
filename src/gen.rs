extern crate genco;

use genco::prelude::*;

use crate::compile::*;
use crate::ir::*;

type GenResult = std::result::Result<Tokens, Box<dyn std::error::Error>>;

trait Gen {
    fn gen(self: &Self) -> GenResult;
}

struct Skeleton<T>(T);

impl Gen for Skeleton<&IrSpec<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(IrSpec { main }) = self;
        Ok(quote! {
            int main() {
                #(Skeleton(main).gen()?)
            }
        })
    }
}

impl Gen for Skeleton<&IrBlock<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(insts) = self;

        Ok(quote! {
            #(for s in insts.iter() join (#<push>) => #(Skeleton(s).gen()?))
        })
    }
}

impl Gen for Skeleton<&Expr<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(expr) = self;
        Ok(match expr {
            Expr::Var {
                def: Def { name, .. },
                ..
            } => quote!(#(name.to_owned())),
            _ => quote!(unsupported_expression),
        })
    }
}

struct ScanfFormat<T>(T);

struct PrintfFormat<T>(T);

struct LeftHandType<T>(T);

impl Gen for ScanfFormat<ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self(ty) = self;
        Ok(match ty {
            ScalarType::I32 | ScalarType::N32 => quote!("%d"),
            ScalarType::I64 | ScalarType::N64 => quote!("%lld"),
        })
    }
}

impl Gen for PrintfFormat<ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self(ty) = self;
        Ok(match ty {
            ScalarType::I32 | ScalarType::N32 => quote!("%d "),
            ScalarType::I64 | ScalarType::N64 => quote!("%lld "),
        })
    }
}

impl Gen for LeftHandType<ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self(ty) = self;
        Ok(match ty {
            ScalarType::I32 | ScalarType::N32 => quote!(int),
            ScalarType::I64 | ScalarType::N64 => quote!(int64_t),
        })
    }
}

impl Gen for Skeleton<&IrInst<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(inst) = self;
        Ok(match inst {
            IrInst::Decl { def, .. } => quote! {
                #(LeftHandType(def.scalar_type_expr.ty).gen()?) #(def.name);
            },
            IrInst::Write { expr, .. } => quote! {
                printf(#(PrintfFormat(expr.ty().scalar_type()).gen()?), #(Skeleton(*expr).gen()?));
            },
            IrInst::Read { def, .. } => quote! {
                scanf(#(ScanfFormat(def.scalar_type_expr.ty).gen()?), &#(def.name));
            },
            IrInst::Call {
                inner:
                    Stmt::Call {
                        name,
                        args,
                        return_value: None,
                        ..
                    },
            } => quote! {
                #(*name)(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            IrInst::Call {
                inner:
                    Stmt::Call {
                        name,
                        args,
                        return_value: Some(return_value),
                        ..
                    },
            } => quote! {
                #(Skeleton(&return_value.expr()).gen()?) = #(*name)(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            _ => unreachable!(),
        })
    }
}

pub fn gen_file(spec: &Spec<'_>) -> Result<String, Box<dyn std::error::Error>> {
    Ok(Skeleton(&spec.build_ir()).gen()?.to_file_string().unwrap())
}
