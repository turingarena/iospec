use super::*;

pub type IrBlock<'a> = Vec<IrInst<'a>>;

impl<'a> CompiledBlock<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        self.stmts.iter().flat_map(|stmt| stmt.build_ir()).collect()
    }
}
