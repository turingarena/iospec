use std::collections::HashMap;

use by_address::ByAddress;

use crate::spec::hir::*;

use super::val::*;

#[derive(Default)]
pub struct RState {
    pub env: HashMap<ByAddress<Rc<HVarDef>>, RNode>,
    pub indexes: HashMap<ByAddress<Rc<HRange>>, usize>,
}
