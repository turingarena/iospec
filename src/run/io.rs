use std::error::Error;
use std::io::Read;
use std::str::FromStr;

use crate::spec::ty::*;

pub trait AtomSource {
    fn next_atom(self: &mut Self, ty: &AtomTy) -> Result<Option<i64>, Box<dyn Error>>;
}

pub struct TextSource<T: Read> {
    pub reader: T,
}

impl<T: Read> AtomSource for TextSource<T> {
    fn next_atom(self: &mut Self, _ty: &AtomTy) -> Result<Option<i64>, Box<dyn Error>> {
        let val = self
            .reader
            .by_ref()
            .bytes()
            .skip_while(|b| match b {
                Ok(b) => b.is_ascii_whitespace(),
                _ => false,
            })
            .take_while(|b| match b {
                Ok(b) => !b.is_ascii_whitespace(),
                _ => false,
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(if val.is_empty() {
            None
        } else {
            Some(i64::from_str(&String::from_utf8(val)?)?)
        })
    }
}
