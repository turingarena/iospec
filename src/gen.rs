extern crate genco;

use genco::prelude::*;

use crate::compile::*;

type GenResult = std::result::Result<Tokens, Box<dyn std::error::Error>>;

trait Gen {
    fn gen(self: &Self) -> GenResult;
}

struct Skeleton<T>(T);

impl Gen for Skeleton<&CompiledSpec<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(CompiledSpec { main, .. }) = self;
        Ok(quote! {
            int main() {
                #(Skeleton(main).gen()?)
            }
        })
    }
}

impl Gen for Skeleton<&CompiledBlock<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(b) = self;

        Ok(quote! {
            #(for s in b.stmts.iter() join (#<push>) => #(Skeleton(s).gen()?))
        })
    }
}

impl Gen for Skeleton<&CompiledExpr<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(expr) = self;
        Ok(match expr {
            CompiledExpr::Var(CompiledExprVar {
                decl: CompiledDecl { name, .. },
                ..
            }) => quote!(#(name.to_owned())),
            _ => quote!(unsupported_expression),
        })
    }
}

struct ScanfTarget<T>(T);

impl Gen for ScanfTarget<&CompiledDecl<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(decl) = self;
        Ok(quote!( #(decl.name) ))
    }
}

struct ScanfFormat<T>(T);
struct PrintfFormat<T>(T);

impl Gen for ScanfFormat<ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self(ty) = self;
        Ok(match ty {
            ScalarType::I32 | ScalarType::N32 => quote!("%d"),
            _ => quote!(invalid format),
        })
    }
}

impl Gen for PrintfFormat<ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self(ty) = self;
        Ok(match ty {
            ScalarType::I32 | ScalarType::N32 => quote!("%d "),
            _ => quote!(invalid format),
        })
    }
}

impl Gen for Skeleton<&CompiledStmt<'_>> {
    fn gen(self: &Self) -> GenResult {
        let Self(stmt) = self;
        Ok(match stmt {
            CompiledStmt::Write(CompiledStmtWrite { args, .. }) => quote! {
                printf(#(
                    for a in args join ( ) => "%d "
                )  r"\n", #(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            CompiledStmt::Read(CompiledStmtRead { args, .. }) => quote! {
                scanf(#(
                    for a in args join ( ) => #(ScanfFormat(a.ty.ty).gen()?)
                ), #(
                    for a in args join (, ) => &#(ScanfTarget(a).gen()?)
                ));
            },
            CompiledStmt::Call(CompiledStmtCall {
                name,
                args,
                return_value: None,
                ..
            }) => quote! {
                #(*name)(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            CompiledStmt::Call(CompiledStmtCall {
                name,
                args,
                return_value: Some(return_value),
                ..
            }) => quote! {
                #(Skeleton(&return_value.expr()).gen()?) = #(*name)(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            _ => quote! {
                bad statement
            },
        })
    }
}

pub fn gen_file(spec: &CompiledSpec<'_>) -> Result<String, Box<dyn std::error::Error>> {
    Ok(Skeleton(spec).gen()?.to_file_string().unwrap())
}
