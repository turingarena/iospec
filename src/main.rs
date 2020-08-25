#![feature(proc_macro_span)]

mod atom;
mod code;
mod opt;
mod run;
mod spec;
mod spec_load;

fn main() -> Result<(), ()> {
    use opt::Opt;
    use spec_load::*;
    use structopt::StructOpt;

    let opt = Opt::from_args();

    match opt {
        Opt::Lint { spec_file } => {
            spec_load(&spec_file)?;
        }

        Opt::Code { spec_file } => {
            use code::*;

            let spec = spec_load(&spec_file)?;
            let code = code_gen(&spec, &code::lang::Cpp);

            print!("{}", code);
        }

        Opt::Run {
            spec_file,
            input_from,
            output_from,
        } => {
            use run::*;
            use std::fs::File;

            let spec = spec_load(&spec_file)?;

            spec_run(
                &spec,
                File::open(input_from).unwrap(),
                File::open(output_from).unwrap(),
            );
        }
    }

    Ok(())
}
