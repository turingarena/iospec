/// Type of an atomic value (semantics)
#[derive(Debug, Clone, Copy)]
pub enum AtomTy {
    /// Boolean, either true or false
    Boolean,
    /// Non-negative integer representable as a *signed* integer of the given bit size
    Natural {
        size: BitSize,
    },
    /// Signed integer representable in the given bit size
    Integer {
        size: BitSize,
    },
    Err,
}

impl AtomTy {
    pub fn all() -> Vec<Self> {
        use AtomTy::*;
        let mut all = Vec::new();
        all.push(Boolean);
        for size in BitSize::all() {
            all.push(AtomTy::Natural { size });
            all.push(AtomTy::Integer { size });
        }
        all
    }

    pub fn name(self: Self) -> String {
        match self {
            AtomTy::Boolean => "bool".into(),
            AtomTy::Natural { size } => format!("n{}", size.bits()),
            AtomTy::Integer { size } => format!("i{}", size.bits()),
            AtomTy::Err => "<invalid scalar type>".into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
}
