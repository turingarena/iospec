use std::collections::HashMap;

use crate::spec::hir::*;

use super::val::*;

#[derive(Debug, Default)]
pub struct RState {
    pub env: HashMap<*const HVarDef, RNode>,
    pub indexes: HashMap<*const HRange, usize>,
}
