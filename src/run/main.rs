use std::fs::File;

use crate::spec::hir::*;
use crate::spec::*;

use super::ctx::*;
use super::err::*;
use super::io::*;
use super::state::*;

pub fn spec_run(spec: &Spec, input_from: File, output_from: File) {
    let mut state = RState::default();
    let mut ctx = Runner {
        input_source: TextSource { reader: input_from },
        output_source: TextSource {
            reader: output_from,
        },
    };

    match spec.hir.run(&mut state, &mut ctx) {
        Ok(_) => (),
        Err(e) => println!("{}", e.diagnostic_message(&state, &spec.sess)),
    }
}
