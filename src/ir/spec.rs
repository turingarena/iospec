use super::*;

pub struct IrSpec<'a> {
    pub main: IrBlock<'a>,
}

impl<'a> Spec<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        self.main.build_ir()
    }
}
