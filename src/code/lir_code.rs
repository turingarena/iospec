use genco::prelude::*;

use super::lir::*;

pub trait Code {
    fn code_into(self: &Self, tokens: &mut Tokens);
}

impl<T: Code, F: LirFlavor> Code for Lir<F, T> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        self.as_ref().code_into(tokens)
    }
}

impl<F: LirFlavor, T> FormatInto<()> for &Lir<F, T>
where
    Lir<F, T>: Code,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(tokens)
    }
}

impl<F: LirFlavor, T> FormatInto<()> for &Box<Lir<F, T>>
where
    Lir<F, T>: Code,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(tokens)
    }
}
