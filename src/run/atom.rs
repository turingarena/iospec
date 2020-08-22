//! Stores atomic values compactly in memory

use std::fmt::Debug;

use num_traits::{Bounded, Num, NumCast};

use crate::spec::ty::*;

/// Compact representation of an atom, to use in array cells
pub trait RAtom: Clone + Copy + Debug + Num + Bounded + NumCast {}

impl RAtom for i8 {}

impl RAtom for i16 {}

impl RAtom for i32 {}

impl RAtom for i64 {}

pub trait RAtomCell: Debug {
    fn get(self: &Self) -> i64;
    fn set(self: &mut Self, value: i64);
}

impl<T: RAtom> RAtomCell for T {
    fn get(self: &Self) -> i64 {
        if *self == Self::min_value() {
            i64::MIN
        } else {
            (*self).to_i64().unwrap()
        }
    }

    fn set(self: &mut Self, value: i64) {
        assert_ne!(value, i64::MIN);
        *self = if value == i64::MIN {
            Self::min_value()
        } else {
            <T as NumCast>::from(value).unwrap()
        }
    }
}

pub trait RAtomArray: Debug {
    fn at(self: &Self, index: usize) -> &dyn RAtomCell;
    fn at_mut(self: &mut Self, index: usize) -> &mut dyn RAtomCell;
}

impl<T: RAtom> RAtomArray for Vec<T> {
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
            AtomTy::Bool => Box::new(i8::MIN),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(i8::MIN),
                BitSize::S16 => Box::new(i16::MIN),
                BitSize::S32 => Box::new(i32::MIN),
                BitSize::S64 => Box::new(i64::MIN),
            },
            AtomTy::Err => unreachable!(),
        }
    }

    pub fn array(self: &Self, len: usize) -> Box<dyn RAtomArray> {
        match self {
            AtomTy::Bool => Box::new(vec![i8::MIN; len]),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(vec![i8::MIN; len]),
                BitSize::S16 => Box::new(vec![i16::MIN; len]),
                BitSize::S32 => Box::new(vec![i32::MIN; len]),
                BitSize::S64 => Box::new(vec![i64::MIN; len]),
            },
            AtomTy::Err => unreachable!(),
        }
    }
}
