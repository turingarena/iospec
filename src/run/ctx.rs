//! How the spec execution interacts with the environment.
//!
//! Includes, e.g., input/output streams.

use std::fs::File;

use super::io::*;

pub struct Runner {
    pub input_source: TextSource<File>,
    pub output_source: TextSource<File>,
}

impl RunContext for Runner {
    type InputSource = TextSource<File>;
    type OutputSource = TextSource<File>;

    fn input_source(self: &mut Self) -> &mut Self::InputSource {
        &mut self.input_source
    }

    fn output_source(self: &mut Self) -> &mut Self::OutputSource {
        &mut self.output_source
    }
}

pub trait RunContext {
    type InputSource: AtomSource;
    type OutputSource: AtomSource;

    fn input_source(self: &mut Self) -> &mut Self::InputSource;
    fn output_source(self: &mut Self) -> &mut Self::OutputSource;
}
