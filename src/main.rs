#![feature(proc_macro_span)]

extern crate structopt;

use crate::old_hir_compile::compile_spec;
use crate::parsefile::load_spec_file;
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
    #[cfg(disable)]
    #[structopt(about = "Generates code (parser, template, etc.)")]
    Gen {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
}

mod ast;
mod old_hir;
mod old_hir_compile;
mod gen;
mod ir;
mod ast_parse;
mod parsefile;
mod kw;
mod hir;
mod hir_compile;
mod mir;
mod mir_build;

fn main() {
    let app = App::from_args();

    match app {
        App::Lint { spec_file } => match load_spec_file(&spec_file) {
            Ok(spec) => {
                println!("Parsed {:?}", spec);
            }
            Err(e) => {
                println!("{}", e);
                exit(1)
            }
        },

        #[cfg(disable)]
        App::Gen { spec_file } => {
            match load_spec_file(&spec_file)
                .and_then(|spec| gen::gen_file(&compile_spec(&spec).unwrap()))
            {
                Ok(content) => {
                    print!("{}", content);
                }
                Err(e) => {
                    println!("{}", e);
                    exit(1)
                }
            }
        }
    }
}
