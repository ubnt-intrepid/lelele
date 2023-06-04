//! Runtime implementation for `lelele` parser generator.

pub mod definition;
pub mod parser;

// internally used by codegen.
#[doc(hidden)]
pub mod _private {
    pub use crate::{
        definition::{ParseAction, ParseActionError, ParseTable},
        parser::{Parser, Token},
    };
    pub use ::phf;
}
