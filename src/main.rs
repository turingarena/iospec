#![feature(proc_macro_span)]

extern crate structopt;

use std::path::PathBuf;
use structopt::StructOpt;
use crate::parse::parse_spec_file;
use std::process::exit;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "turingarena-iospec",
    about = "Automate validation of input/output, parser generation, and more."
)]
enum App {
    #[structopt(
        about = "Checks that an I/O spec file is correct"
    )]
    Lint {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
    #[structopt(
        about = "Generates code (parser, template, etc.)"
    )]
    Gen {
        #[structopt(long, parse(from_os_str), default_value = "./iospec")]
        spec_file: PathBuf,
    },
}

mod ast;
mod parse;
mod gen;

fn main() {
    let app = App::from_args();

    match app {
        App::Lint {
            spec_file,
        } => {
            match parse_spec_file(&spec_file) {
                Ok(spec) => {
                    println!("Parsed {:?}", spec);
                }
                Err(e) => {
                    println!("{}", e);
                    exit(1)
                }
            }
        },

        App::Gen {
            spec_file
        } => {
            match parse_spec_file(&spec_file).and_then(|spec| gen::gen_file(&spec)) {
                Ok(content) => {
                    print!("{}", content);
                }
                Err(e) => {
                    println!("{}", e);
                    exit(1)
                }
            }
        },
    }
}
