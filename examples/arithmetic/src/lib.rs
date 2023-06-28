pub mod parser {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}
pub mod lexer;
pub mod syntax;
