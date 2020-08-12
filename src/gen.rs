extern crate genco;

use genco::prelude::*;

use crate::ast::*;

type GenResult = std::result::Result<Tokens, Box<dyn std::error::Error>>;

trait Gen {
    fn gen(self: &Self) -> GenResult;
}

struct Skeleton<T>(T);

impl Gen for Skeleton<&Spec> {
    fn gen(self: &Self) -> GenResult {
        let Self (Spec { main }) = self;
        Ok(quote! {
            int main() {
                #(Skeleton(main).gen()?)
            }
        })
    }
}

impl Gen for Skeleton<&Block> {
    fn gen(self: &Self) -> GenResult {
        let Self (b) = self;

        Ok(match b {
            Block::Empty(_) => quote!(),
            Block::Cons(BlockCons { prev, stmt }) => quote!{
                #(Skeleton(prev.as_ref()).gen()?)
                #(Skeleton(stmt).gen()?)
            }
        })
    }
}

impl Gen for Skeleton<&Expr> {
    fn gen(self: &Self) -> GenResult {
        let Self(expr) = self;
        Ok(match expr {
            Expr::Var(ExprVar { ident: Ident { sym }}) => quote!(#sym),
            _ => quote!(unsupported_expression)
        })
    }
}

struct ScanfTarget<T>(T);

impl Gen for ScanfTarget<&Decl> {
    fn gen(self: &Self) -> GenResult {
        let Self(decl) = self;
        Ok(Skeleton(&decl.expr).gen()?)
    }
}

struct ScanfFormat<T>(T);
struct PrintfFormat<T>(T);

impl Gen for ScanfFormat<&ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self (ty) = self;
        Ok(match ty.ident.sym.as_str() {
            "i32" | "n32" => quote!("%d"),
            _ => quote!(invalid format)
        })
    }
}

impl Gen for PrintfFormat<&ScalarType> {
    fn gen(self: &Self) -> GenResult {
        let Self (ty) = self;
        Ok(match ty.ident.sym.as_str() {
            "i32" => quote!("%d "),
            _ => quote!(invalid format)
        })
    }
}

impl Gen for Skeleton<&Stmt> {
    fn gen(self: &Self) -> GenResult {
        let Self(stmt) = self;
        Ok(match stmt {
            Stmt::Write(StmtWrite { args, .. }) => quote! {
                printf(#(
                    for a in args join ( ) => "%d "
                )  r"\n", #(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            Stmt::Read(StmtRead { args, .. }) => quote! {
                scanf(#(
                    for a in args join ( ) => #(ScanfFormat(&a.ty).gen()?)
                ), #(
                    for a in args join (, ) => &#(ScanfTarget(a).gen()?)
                ));
            },
            Stmt::Call(StmtCall { name: Ident { sym: name }, args, return_value: None,.. }) => quote! {
                #name(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            Stmt::Call(StmtCall { name: Ident { sym: name }, args, return_value: Some((_, return_value)),.. }) => quote! {
                #(Skeleton(&return_value.expr).gen()?) = #name(#(
                    for a in args join (, ) => #(Skeleton(a).gen()?)
                ));
            },
            _ => quote! {
                bad statement
            },
        })
    }
}

pub fn gen_file(spec: &Spec) -> Result<String, Box<dyn std::error::Error>> {
    Ok(Skeleton(spec).gen()?.to_file_string().unwrap())
}