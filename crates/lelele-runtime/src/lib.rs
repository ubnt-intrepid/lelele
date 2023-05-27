//! Runtime implementation for `lelele` parser generator.

pub mod parser;

// internally used by codegen.
#[doc(hidden)]
pub mod _private {
    pub use crate::parser::{Parser, ParserAction, ParserActionKind, ParserDefinition, Token};
    pub use ::phf;
}
