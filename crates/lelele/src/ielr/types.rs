//! Utility types.

use std::{collections::VecDeque, hash::Hash};

type BuildHasher = std::hash::BuildHasherDefault<rustc_hash::FxHasher>;

pub type Map<K, V> = indexmap::IndexMap<K, V, BuildHasher>;
pub type Set<T> = indexmap::IndexSet<T, BuildHasher>;

#[derive(Debug)]
pub struct Queue<T> {
    queue: VecDeque<T>,
    hash: Set<T>,
}
impl<T> Default for Queue<T> {
    fn default() -> Self {
        Self {
            queue: VecDeque::new(),
            hash: Set::default(),
        }
    }
}

impl<T> Queue<T>
where
    T: Clone + Eq + Hash,
{
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            queue: VecDeque::with_capacity(capacity),
            hash: Set::with_capacity_and_hasher(capacity, Default::default()),
        }
    }

    pub fn push(&mut self, value: T) {
        if self.hash.insert(value.clone()) {
            self.queue.push_back(value);
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        let value = self.queue.pop_front()?;
        self.hash.remove(&value);
        Some(value)
    }
}

impl<T> FromIterator<T> for Queue<T>
where
    T: Clone + Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut queue = Self::default();
        for value in iter {
            queue.push(value);
        }
        queue
    }
}
