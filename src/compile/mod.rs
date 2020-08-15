pub use block::*;
pub use def::*;
pub use expr::*;
pub use range::*;
use scope::*;
pub use spec::*;
pub use stmt::*;
pub use ty::*;

use crate::ast::*;

mod scope;

type CompileResult<T> = Result<T, String>;

trait Compile<'ast, T>
where
    Self: std::marker::Sized,
{
    fn compile(ast: &'ast T, scope: &Scope<'ast>) -> CompileResult<Self>;
}

fn compile<'ast, T, U>(ast: &'ast T, scope: &Scope<'ast>) -> CompileResult<U>
where
    U: Compile<'ast, T>,
{
    U::compile(ast, scope)
}

mod block;
mod def;
mod expr;
mod range;
mod spec;
mod stmt;
mod ty;
