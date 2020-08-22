use std::fs::File;

use crate::spec::hir::*;

use super::interp::*;
use super::io::*;

pub fn spec_run(spec: &Rc<HSpec>, input_from: File, output_from: File) {
    let mut state = RState::default();
    let mut ctx = Runner {
        input_parser: TextAtomStream { reader: input_from },
        output_parser: TextAtomStream {
            reader: output_from,
        },
    };

    spec.run(&mut state, &mut ctx).unwrap();
}
