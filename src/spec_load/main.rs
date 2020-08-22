use std::fs::read_to_string;
use std::path::Path;

use crate::spec::hir::*;
use crate::spec_load::ast_parse::parse_spec;
use crate::spec_load::hir_compile::compile_hir;

use super::sess::Sess;

/// Loads the spec at the given path.
/// Prints errors on stderr.
pub fn spec_load(path: &Path) -> Result<Rc<HSpec>, ()> {
    let mut code_map = codemap::CodeMap::new();

    let file = code_map.add_file(
        path.to_str().expect("file path is not valid UTF-8").into(),
        read_to_string(path).expect("cannot read file"),
    );

    let sess = Sess::new(&file);

    let mut dgns = Vec::new();

    let spec = parse_spec(sess.file.clone().source(), &mut dgns)
        .and_then(|spec| compile_hir(spec, &mut dgns));

    for d in dgns {
        eprintln!("{}", d.diagnostic_message(&sess));
    }

    Ok(spec?)
}
