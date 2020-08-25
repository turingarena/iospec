use genco::prelude::*;

use super::lir::*;
use super::lir_code::*;
use crate::spec::rel::RelOp;

/// Wraps any LIR node, so that it is translated as usual in many programming languages.
pub struct CommonLang<'a, T>(pub &'a T);

impl<L: CodeLang> Code<L> for CommonLang<'_, LBlock<L>>
where
    LStmt<L>: Code<L>,
{
    fn code_into(self: &Self, _lang: &L, tokens: &mut Tokens) {
        let LBlock { stmts } = &self.0;
        quote_in! { *tokens =>
            #(for stmt in stmts join (#<line>) => #stmt)
        }
    }
}

impl<L: CodeLang> Code<L> for CommonLang<'_, LExpr<L>>
where
    LExpr<L>: Code<L>,
    LSign: Code<L>,
    RelOp: Code<L>,
{
    fn code_into(self: &Self, _lang: &L, tokens: &mut Tokens) {
        match &self.0 {
            LExpr::Var { name } => quote_in! { *tokens =>
                #name
            },
            LExpr::Subscript { array, index } => quote_in! { *tokens =>
                #array[#index]
            },
            LExpr::Lit { value } => quote_in! { *tokens =>
                #(*value)
            },
            LExpr::Paren { inner } => quote_in! { *tokens =>
                (#inner)
            },
            LExpr::Mul { factors } => quote_in! { *tokens =>
                #(for f in factors join ( * ) => #f)
            },
            LExpr::Sum { terms } => quote_in! { *tokens =>
                #(for (sign, t) in terms join ( ) => #sign #t)
            },
            LExpr::Rel { left, op, right } => quote_in! { *tokens =>
                #left #op #right
            },
            LExpr::And { clauses } => quote_in! { *tokens =>
                #(for clause in clauses join ( && ) => #clause)
            },
        }
    }
}

impl<L: CodeLang> Code<L> for CommonLang<'_, LSign> {
    fn code_into(self: &Self, _lang: &L, tokens: &mut Tokens) {
        match &self.0 {
            LSign::ImplicitPlus => (),
            LSign::Plus => quote_in!(*tokens => +),
            LSign::Minus => quote_in!(*tokens => -),
        }
    }
}

impl<L: CodeLang> Code<L> for CommonLang<'_, RelOp> {
    fn code_into(self: &Self, _lang: &L, tokens: &mut Tokens) {
        quote_in!(*tokens => #(self.0.to_string()))
    }
}
