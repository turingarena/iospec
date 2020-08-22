#![feature(proc_macro_span)]

extern crate structopt;

use std::fs::{read_to_string, File};
use std::path::PathBuf;

use structopt::StructOpt;

use crate::ast_parse::parse_spec;
use crate::hir_compile::compile_hir;
use crate::lir_build::build_lir;
use crate::run::run_spec;
use crate::sess::Sess;
use std::process::exit;

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

mod diagnostic;
mod sess;

mod ast;
mod ast_parse;
mod kw;

mod ty;

mod hir;
mod hir_compile;
mod hir_env;
mod hir_err;
mod hir_quote;
mod hir_sem;
mod hir_span;

mod lir;
mod lir_build;

mod code;

mod io;
mod run;

fn create_sess(spec_file: &PathBuf) -> Sess {
    let mut code_map = codemap::CodeMap::new();

    let file = code_map.add_file(
        spec_file
            .to_str()
            .expect("file path is not valid UTF-8")
            .into(),
        read_to_string(spec_file).expect("cannot read file"),
    );

    Sess::new(&file)
}

fn main() {
    let app = App::from_args();

    match app {
        App::Lint { spec_file } => {
            let sess = create_sess(&spec_file);
            let mut dgns = Vec::new();

            parse_spec(sess.file.clone().source(), &mut dgns)
                .and_then(|spec| compile_hir(spec, &mut dgns))
                .ok();

            for d in dgns {
                eprintln!("{}", d.diagnostic_message(&sess));
            }
        }

        App::Code { spec_file } => {
            let sess = create_sess(&spec_file);
            let mut dgns = Vec::new();

            let generated = parse_spec(sess.file.clone().source(), &mut dgns)
                .and_then(|spec| compile_hir(spec, &mut dgns))
                .map(|spec| build_lir(spec))
                .map(|spec| code::gen_file(&spec));

            for d in dgns {
                eprintln!("{}", d.diagnostic_message(&sess));
            }

            if let Ok(generated) = generated {
                print!("{}", generated);
            }
        }

        App::Run {
            spec_file,
            input_from,
            output_from,
        } => {
            let sess = create_sess(&spec_file);
            let mut dgns = Vec::new();

            let spec = parse_spec(sess.file.clone().source(), &mut dgns)
                .and_then(|spec| compile_hir(spec, &mut dgns));

            for d in dgns {
                eprintln!("{}", d.diagnostic_message(&sess));
            }

            let spec = match spec {
                Ok(spec) => spec,
                Err(_) => {
                    eprintln!("aborting due to previous errors");
                    exit(1)
                }
            };

            run_spec(
                &spec,
                File::open(input_from).unwrap(),
                File::open(output_from).unwrap(),
            );
        }
    }
}
