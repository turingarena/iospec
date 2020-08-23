use std::error::Error;
use std::io::Read;
use std::str::FromStr;

use crate::spec::ty::*;

pub trait AtomSource {
    fn next_atom(self: &mut Self, ty: &AtomTy) -> Result<i64, Box<dyn Error>>;
}

pub struct TextSource<T: Read> {
    pub reader: T,
}

impl<T: Read> AtomSource for TextSource<T> {
    fn next_atom(self: &mut Self, _ty: &AtomTy) -> Result<i64, Box<dyn Error>> {
        let val = self
            .reader
            .by_ref()
            .bytes()
            .take_while(|b| match b {
                Ok(b) if !b.is_ascii_whitespace() => true,
                _ => false,
            })
            .collect::<Result<Vec<_>, _>>()?;
        let val = i64::from_str(&String::from_utf8(val)?)?;
        Ok(val)
    }
}
