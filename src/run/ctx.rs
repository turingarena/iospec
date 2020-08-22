//! How the spec execution interacts with the environment.
//!
//! Includes, e.g., input/output streams.

use std::fs::File;

use super::io::*;

pub struct Runner {
    pub input_parser: TextAtomStream<File>,
    pub output_parser: TextAtomStream<File>,
}

impl RunContext for Runner {
    type InputStream = TextAtomStream<File>;
    type OutputStream = TextAtomStream<File>;

    fn input_parser(self: &mut Self) -> &mut Self::InputStream {
        &mut self.input_parser
    }

    fn output_parser(self: &mut Self) -> &mut Self::OutputStream {
        &mut self.output_parser
    }
}

pub trait RunContext {
    type InputStream: AtomStream;
    type OutputStream: AtomStream;

    fn input_parser(self: &mut Self) -> &mut Self::InputStream;
    fn output_parser(self: &mut Self) -> &mut Self::OutputStream;
}
