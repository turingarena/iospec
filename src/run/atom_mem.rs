//! Stores atomic values compactly in memory

use std::fmt::Debug;

use num_traits::{Bounded, Num, NumCast};

use crate::run::atom::RAtom;
use crate::spec::ty::*;

pub trait RUninit {
    fn uninit() -> Self;
}

impl<T: Bounded> RUninit for T {
    fn uninit() -> Self {
        Self::min_value()
    }
}

/// Compact representation of an atom, to use in array cells
pub trait RAtomMem: Clone + Copy + Debug + Num + RUninit + NumCast {}

impl RAtomMem for i8 {}

impl RAtomMem for i16 {}

impl RAtomMem for i32 {}

impl RAtomMem for i64 {}

pub trait RAtomCell: Debug {
    fn get(self: &Self, ty: AtomTy) -> Option<RAtom>;
    fn set(self: &mut Self, value: RAtom);
}

impl<T: RAtomMem> RAtomCell for T {
    fn get(self: &Self, ty: AtomTy) -> Option<RAtom> {
        if *self == Self::uninit() {
            None
        } else {
            Some(RAtom::new(ty, (*self).to_i64().unwrap()))
        }
    }

    fn set(self: &mut Self, value: RAtom) {
        *self = <T as NumCast>::from(value.value_i64()).unwrap()
    }
}

pub trait RAtomArray: Debug {
    fn at(self: &Self, index: usize) -> &dyn RAtomCell;
    fn at_mut(self: &mut Self, index: usize) -> &mut dyn RAtomCell;
}

impl<T: RAtomMem> RAtomArray for Vec<T> {
    fn at(self: &Self, index: usize) -> &dyn RAtomCell {
        &self[index]
    }

    fn at_mut(self: &mut Self, index: usize) -> &mut dyn RAtomCell {
        &mut self[index]
    }
}

impl AtomTy {
    pub fn cell(self: &Self) -> Box<dyn RAtomCell> {
        match self {
            AtomTy::Bool => Box::new(i8::uninit()),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(i8::uninit()),
                BitSize::S16 => Box::new(i16::uninit()),
                BitSize::S32 => Box::new(i32::uninit()),
                BitSize::S64 => Box::new(i64::uninit()),
            },
            AtomTy::Err => unreachable!(),
        }
    }

    pub fn array(self: &Self, len: usize) -> Box<dyn RAtomArray> {
        match self {
            AtomTy::Bool => Box::new(vec![i8::uninit(); len]),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(vec![i8::uninit(); len]),
                BitSize::S16 => Box::new(vec![i16::uninit(); len]),
                BitSize::S32 => Box::new(vec![i32::uninit(); len]),
                BitSize::S64 => Box::new(vec![i64::uninit(); len]),
            },
            AtomTy::Err => unreachable!(),
        }
    }
}
