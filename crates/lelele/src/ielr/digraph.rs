use crate::types::Map;
use indexmap::map::Slice;
use std::{cmp, hash::Hash};

pub trait Set {
    fn union_with(&mut self, other: &Self);
}

impl<T> Set for crate::types::Set<T>
where
    T: Clone + Eq + Hash,
{
    fn union_with(&mut self, other: &Self) {
        self.extend(other.iter().cloned())
    }
}

impl<B> Set for bit_set::BitSet<B>
where
    B: bit_vec::BitBlock,
{
    fn union_with(&mut self, other: &Self) {
        self.union_with(other)
    }
}

/// Calculate
pub fn digraph<K, T>(result: &mut Map<K, T>, relation: impl Fn(&K, &K) -> bool)
where
    K: Clone + Eq + Hash,
    T: Set,
{
    let keys: Vec<_> = result.keys().cloned().collect();
    Digraph {
        result: result.as_mut_slice(),
        relation,
        keys: &keys[..],
        n: vec![0usize; keys.len()],
        stack: vec![],
    }
    .run()
}

struct Digraph<'a, K, T, F> {
    result: &'a mut Slice<K, T>,
    relation: F,
    keys: &'a [K],
    n: Vec<usize>,
    stack: Vec<usize>,
}

impl<K, T, F> Digraph<'_, K, T, F>
where
    K: Eq + Hash,
    T: Set,
    F: Fn(&K, &K) -> bool,
{
    fn run(&mut self) {
        for x in 0..self.keys.len() {
            if self.n[x] == 0 {
                self.traverse(x);
            }
        }
    }

    fn traverse(&mut self, x: usize) {
        self.stack.push(x);
        let d = self.stack.len();
        self.n[x] = d;

        let x_key = &self.keys[x];
        for (y, y_key) in self.keys.iter().enumerate() {
            if !(self.relation)(x_key, y_key) {
                continue;
            }

            if self.n[y] == 0 {
                self.traverse(y);
            }
            self.n[x] = cmp::min(self.n[x], self.n[y]);

            if x != y {
                // F(x) <- F(x) \cup F(y)
                let (slot, added) = get_two_mut(&mut self.result, x, y);
                slot.union_with(added);
            }
        }

        if self.n[x] != d {
            return;
        }

        while let Some(s) = self.stack.pop() {
            self.n[s] = usize::MAX;
            if s == x {
                break;
            }
            // F(s) <- F(x)
            let (slot, added) = get_two_mut(&mut self.result, s, x);
            slot.union_with(added);
        }
    }
}

fn get_two_mut<K, V>(slice: &mut Slice<K, V>, x: usize, y: usize) -> (&mut V, &mut V) {
    assert!(
        x != y && cmp::max(x, y) < slice.len(),
        "index condition not satisfied"
    );
    let i = (x + y) / 2 + 1;
    let (a, b) = slice.split_at_mut(i);
    if x < y {
        (&mut a[x], &mut b[y - i])
    } else {
        (&mut b[x - i], &mut a[y])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_two_mut() {
        let mut map = Map::default();
        map.insert("a", "a");
        map.insert("b", "b");
        map.insert("c", "c");
        map.insert("d", "d");
        let slice = map.as_mut_slice();
        assert!(matches!(get_two_mut(slice, 0, 1), t if *t.0=="a" && *t.1=="b"));
        assert!(matches!(get_two_mut(slice, 1, 0), t if *t.0=="b" && *t.1=="a"));
        assert!(matches!(get_two_mut(slice, 0, 2), t if *t.0=="a" && *t.1=="c"));
        assert!(matches!(get_two_mut(slice, 2, 1), t if *t.0=="c" && *t.1=="b"));
        assert!(matches!(get_two_mut(slice, 3, 2), t if *t.0=="d" && *t.1=="c"));
    }
}
