//! Syntax support for Lelele grammar file.

pub mod build;
pub mod codegen;
pub mod dfa;
pub mod grammar;
pub mod parser;

// type shortcut
type BuildFxHasher = std::hash::BuildHasherDefault<rustc_hash::FxHasher>;
type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildFxHasher>;
type IndexSet<T> = indexmap::IndexSet<T, BuildFxHasher>;
