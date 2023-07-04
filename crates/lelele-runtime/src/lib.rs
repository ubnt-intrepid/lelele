//! Runtime implementation for `lelele` parser generator.

pub mod definition;
pub mod engine;

// internally used by codegen.
#[doc(hidden)]
pub mod _private {
    pub use crate::{
        definition::{ParseAction, ParserDef, Terminal},
        engine::{ParseEngine, Token},
    };
}
