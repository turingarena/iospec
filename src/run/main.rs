use std::fs::File;

use crate::spec::hir::*;

use super::ctx::*;
use super::err::*;
use super::io::*;
use super::state::*;

pub fn spec_run(spec: &Rc<HSpec>, input_from: File, output_from: File) {
    let mut state = RState::default();
    let mut ctx = Runner {
        input_source: TextSource { reader: input_from },
        output_source: TextSource {
            reader: output_from,
        },
    };

    spec.run(&mut state, &mut ctx)
        .map_err(|e| match e {
            RError::UnresolvedVal { .. } => "Unresolved val",
            RError::InvalidInputAtom { .. } => "Invalid input atom",
            RError::InvalidOutputAtom { .. } => "Invalid output atom",
        })
        .unwrap();
}
