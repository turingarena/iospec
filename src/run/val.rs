//! Store data values

use super::atom::*;
use super::atom_mem::*;

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

#[derive(Debug)]
pub enum RVal<'a> {
    Atom(RAtom),
    Aggr(&'a RAggr),
}

#[derive(Debug)]
pub enum RValMut<'a> {
    Atom(&'a mut dyn RAtomCell),
    Aggr(&'a mut RAggr),
    ConstAtom(RAtom),
}
