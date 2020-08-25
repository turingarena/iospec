use genco::prelude::*;

use super::lir::*;

pub trait Code<L: CodeLang> {
    fn code_into(self: &Self, lang: &L, tokens: &mut Tokens);
}

impl<T: Code<L>, L: CodeLang> Code<L> for Lir<L, T> {
    fn code_into(self: &Self, lang: &L, tokens: &mut Tokens) {
        self.as_ref().code_into(lang, tokens)
    }
}

impl<L: CodeLang, T> FormatInto<()> for &Lir<L, T>
where
    Lir<L, T>: Code<L>,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(&self.lang, tokens)
    }
}

impl<L: CodeLang, T> FormatInto<()> for &Box<Lir<L, T>>
where
    Lir<L, T>: Code<L>,
{
    fn format_into(self, tokens: &mut Tokens) {
        self.code_into(&self.lang, tokens)
    }
}
