//! Store data values

use super::atom::*;

#[derive(Debug)]
pub enum RNode {
    Atom(Box<dyn RAtomCell>),
    Aggr(RAggr),
}

#[derive(Debug)]
pub enum RAggr {
    AtomArray(Box<dyn RAtomArray>),
    AggrArray(Vec<RAggr>),
    Unalloc,
}

#[derive(Clone, Copy, Debug)]
pub enum RVal<'a> {
    Atom(i64),
    Aggr(&'a RAggr),
}

#[derive(Debug)]
pub enum RValMut<'a> {
    Atom(&'a mut dyn RAtomCell),
    Aggr(&'a mut RAggr),
    ConstAtom(i64),
}
