use std::sync::Arc;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::*;
use codemap::File;
use proc_macro2::{LineColumn, Span};

#[derive(Debug)]
pub struct Sess {
    pub file: Arc<File>,
}

impl Sess {
    pub fn new(file: &Arc<File>) -> Sess {
        Sess { file: file.clone() }
    }
}

impl Sess {
    fn pos(self: &Self, lc: LineColumn) -> usize {
        let line_start = self.file.line_span(lc.line - 1).low() - self.file.span.low();
        line_start as usize + lc.column
    }

    pub fn error_ann<'a>(self: &'a Self, label: &'a str, span: Span) -> SourceAnnotation {
        SourceAnnotation {
            annotation_type: AnnotationType::Error,
            label,
            range: (self.pos(span.start()), self.pos(span.end())),
        }
    }

    pub fn help_ann<'a>(self: &'a Self, label: &'a str, span: Span) -> SourceAnnotation {
        SourceAnnotation {
            annotation_type: AnnotationType::Info,
            label,
            range: (self.pos(span.start()), self.pos(span.end())),
        }
    }

    pub fn error(self: &Self, message: &str, annotations: Vec<SourceAnnotation>) -> String {
        let snippet = Snippet {
            title: Some(Annotation {
                id: None,
                label: Some(message),
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![Slice {
                source: self.file.source(),
                line_start: 1,
                origin: Some(self.file.name()),
                fold: false,
                annotations,
            }],
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };

        DisplayList::from(snippet).to_string()
    }
}
