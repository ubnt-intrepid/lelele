//! Syntax definition.

use indexmap::IndexMap;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct EscapedStr(str);

impl EscapedStr {
    pub const fn from_raw<'a>(raw: &'a str) -> &'a Self {
        unsafe { &*(raw as *const _ as *const Self) }
    }
}

#[derive(Debug)]
pub enum Value<'source> {
    Null,
    Bool(bool),
    Number(&'source str),
    String(&'source EscapedStr),
    Array(Array<'source>),
    Object(Object<'source>),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => f.write_str(n),
            Self::String(s) => write!(f, "\"{}\"", &s.0),
            Self::Array(arr) => fmt::Display::fmt(arr, f),
            Self::Object(obj) => fmt::Display::fmt(obj, f),
        }
    }
}

#[derive(Debug)]
pub struct Array<'source> {
    pub elements: Vec<Box<Value<'source>>>,
}
impl fmt::Display for Array<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;
        for (i, elem) in self.elements.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            elem.fmt(f)?;
        }
        f.write_str("]")
    }
}

#[derive(Debug)]
pub struct Object<'source> {
    pub members: IndexMap<&'source EscapedStr, Box<Value<'source>>>,
}
impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{")?;
        for (i, (key, value)) in self.members.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            write!(f, "\"{}\": {}", &key.0, value)?;
        }
        f.write_str("}")
    }
}
