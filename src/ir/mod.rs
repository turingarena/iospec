//! Intermediate representation (IR)
//!
//! Compiled AST is expanded into IR, adding variable declarations,
//! construction and destruction, array allocations and de-allocations,
//! and expanding reads and writes.

pub use block::*;
pub use inst::*;
pub use spec::*;

use crate::old_hir::*;

mod block;
mod inst;
mod spec;
