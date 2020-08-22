extern crate structopt;

use std::path::PathBuf;

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "turingarena-iospec",
    about = "Automate validation of input/output, parser generation, and more."
)]
pub enum Opt {
    #[structopt(about = "Checks that an I/O spec file is correct")]
    Lint {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
    #[structopt(about = "Generates code (parser, template, etc.)")]
    Code {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
    #[structopt(about = "Runs the spec to check and/or transform an input and/or output")]
    Run {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
        #[structopt(long, parse(from_os_str), default_value = "./input.txt")]
        input_from: PathBuf,
        #[structopt(long, parse(from_os_str), default_value = "./output.txt")]
        output_from: PathBuf,
    },
}
