#![feature(proc_macro_span)]

extern crate structopt;

use std::fs::File;
use std::path::PathBuf;

use structopt::StructOpt;

use crate::code::code_gen;
use crate::run::interp::run_spec;
use crate::spec_load::*;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "turingarena-iospec",
    about = "Automate validation of input/output, parser generation, and more."
)]
enum App {
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

mod code;
mod run;
mod spec;
mod spec_load;

fn main() -> Result<(), ()> {
    let app = App::from_args();

    match app {
        App::Lint { spec_file } => {
            spec_load(&spec_file)?;
        }

        App::Code { spec_file } => {
            let spec = spec_load(&spec_file)?;
            let code = code_gen(&spec);
            print!("{}", code);
        }

        App::Run {
            spec_file,
            input_from,
            output_from,
        } => {
            let spec = spec_load(&spec_file)?;

            run_spec(
                &spec,
                File::open(input_from).unwrap(),
                File::open(output_from).unwrap(),
            );
        }
    }

    Ok(())
}
