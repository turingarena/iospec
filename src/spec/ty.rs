/// Type of an atomic value (semantics)
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum AtomTy {
    /// Boolean, either true or false
    Bool,
    /// Non-negative integer representable as a *signed* integer of the given bit size
    Nat {
        size: BitSize,
    },
    /// Signed integer representable in the given bit size
    Int {
        size: BitSize,
    },
    Err,
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
            AtomTy::Err => "<invalid scalar type>".into(),
        }
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
