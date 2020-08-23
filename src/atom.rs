use std::str::FromStr;

/// Type of an atomic value (semantics)
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum AtomTy {
    /// Boolean, either true or false
    Bool,
    /// Non-negative integer representable as a *signed* integer of the given bit size
    Nat { size: BitSize },
    /// Signed integer representable in the given bit size
    Int { size: BitSize },
}

impl AtomTy {
    pub fn all() -> Vec<Self> {
        use AtomTy::*;
        let mut all = Vec::new();
        all.push(Bool);
        for size in BitSize::all() {
            all.push(AtomTy::Nat { size });
            all.push(AtomTy::Int { size });
        }
        all
    }

    pub fn name(self: Self) -> String {
        match self {
            AtomTy::Bool => "bool".into(),
            AtomTy::Nat { size } => format!("n{}", size.bits()),
            AtomTy::Int { size } => format!("i{}", size.bits()),
        }
    }
}

impl FromStr for AtomTy {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::all().into_iter().find(|k| &k.name() == s).ok_or(())
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BitSize {
    S8,
    S16,
    S32,
    S64,
}

impl BitSize {
    pub fn all() -> Vec<Self> {
        use BitSize::*;
        vec![S8, S16, S32, S64]
    }

    pub fn bits(self: Self) -> u8 {
        use BitSize::*;
        match self {
            S8 => 8,
            S16 => 16,
            S32 => 32,
            S64 => 64,
        }
    }

    pub fn max_safe_value(self: Self) -> i64 {
        (1 << (self.bits() as i64 - 1)) - 1
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    ty: AtomTy,
    value: i64,
}

impl Atom {
    pub fn new(ty: AtomTy, value: i64) -> Atom {
        Self::try_new(ty, value).unwrap()
    }

    pub fn try_new(ty: AtomTy, value: i64) -> Result<Atom, AtomTypeError> {
        let ok = match ty {
            AtomTy::Bool => value == 0 || value == 1,
            AtomTy::Nat { size } => 0 <= value && value <= size.max_safe_value(),
            AtomTy::Int { size } => {
                -size.max_safe_value() <= value && value <= size.max_safe_value()
            }
        };

        if ok {
            Ok(Atom { ty, value })
        } else {
            Err(AtomTypeError { ty, actual: value })
        }
    }

    pub fn value_i64(self: &Self) -> i64 {
        self.value
    }

    pub fn ty(self: &Self) -> AtomTy {
        self.ty
    }
}

#[derive(Debug)]
pub struct AtomTypeError {
    pub ty: AtomTy,
    pub actual: i64,
}
