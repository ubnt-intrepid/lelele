//! An LR(1) parser generator.

pub mod codegen;
pub mod dfa;
pub mod grammar;

// type shortcut
type BuildFxHasher = std::hash::BuildHasherDefault<rustc_hash::FxHasher>;
type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildFxHasher>;
type IndexSet<T> = indexmap::IndexSet<T, BuildFxHasher>;
