//! Syntax support for Lelele grammar file.

pub mod build;
pub mod codegen;
pub mod grammar;
pub mod lr1;
pub mod reachability;
pub mod syntax;
pub mod util;

// type shortcut
type BuildFxHasher = std::hash::BuildHasherDefault<rustc_hash::FxHasher>;
type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildFxHasher>;
type IndexSet<T> = indexmap::IndexSet<T, BuildFxHasher>;
