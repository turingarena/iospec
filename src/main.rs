#![feature(proc_macro_span)]

extern crate structopt;

use crate::hir_compile::compile_hir;
use crate::mir_build::build_mir;
use crate::parsefile::parse_file;
use std::path::PathBuf;
use std::process::exit;
use structopt::StructOpt;

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
    Gen {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
}

mod ast;
mod ast_parse;
mod gen;
mod hir;
mod hir_analyze;
mod hir_compile;
mod hir_env;
mod kw;
mod mir;
mod mir_build;
mod parsefile;

fn main() {
    let app = App::from_args();

    match app {
        App::Lint { spec_file } => match parse_file(&spec_file) {
            Ok(spec) => {
                println!("Parsed {:?}", spec);
            }
            Err(e) => {
                println!("{}", e);
                exit(1)
            }
        },

        App::Gen { spec_file } => {
            let spec = parse_file(&spec_file).unwrap();
            let spec = compile_hir(spec);
            let spec = build_mir(&spec);
            let spec = gen::gen_file(&spec);

            print!("{}", spec);
        }
    }
}
