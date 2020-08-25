use genco::prelude::*;

use super::lir::*;

pub trait Code {
    fn code_into(self: &Self, tokens: &mut Tokens);
}

impl<T: Code, C: LirConfig> Code for Lir<C, T> {
    fn code_into(self: &Self, tokens: &mut Tokens) {
        self.as_ref().code_into(tokens)
    }
}

impl<C: LirConfig, T> FormatInto<()> for &Lir<C, T>
where
    Lir<C, T>: Code,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(tokens)
    }
}

impl<C: LirConfig, T> FormatInto<()> for &Box<Lir<C, T>>
where
    Lir<C, T>: Code,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(tokens)
    }
}
