extern crate annotate_snippets;
extern crate codemap;
extern crate proc_macro2;
extern crate syn;

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::read_to_string;
use std::path::Path;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::*;

use crate::ast::AstSpec;

#[derive(Debug)]
pub struct ParseError;

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl Error for ParseError {}

pub fn parse_file(path: &Path) -> Result<AstSpec, Box<dyn Error>> {
    let mut code_map = codemap::CodeMap::new();

    let file = code_map.add_file(
        path.to_str().expect("file path is not valid UTF-8").into(),
        read_to_string(path)?,
    );

    let res = syn::parse_str::<AstSpec>(file.source());

    match res {
        Err(e) => {
            let start_loc = e.span().start();
            let end_loc = e.span().end();
            let message = e.to_string();

            let display_list = DisplayList::from(Snippet {
                title: Some(Annotation {
                    id: None,
                    label: Some(message.as_str()),
                    annotation_type: AnnotationType::Error,
                }),
                footer: vec![],
                slices: vec![Slice {
                    source: file.source(),
                    line_start: 1,
                    origin: None,
                    fold: false,
                    annotations: vec![SourceAnnotation {
                        range: (
                            (file.line_span(start_loc.line - 1).low() - file.span.low()) as usize
                                + start_loc.column,
                            (file.line_span(end_loc.line - 1).low() - file.span.low()) as usize
                                + end_loc.column,
                        ),
                        annotation_type: AnnotationType::Error,
                        label: "here",
                    }],
                }],
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            });

            print!("{}", display_list);

            Err(Box::new(ParseError))
        }
        Ok(spec) => Ok(spec)
    }
}
