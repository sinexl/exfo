use crate::common::Stack;
use std::collections::HashMap;
use std::fmt::Display;

pub trait GetAt<K, V> {
    fn get_at(&self, key: &K, depth: usize) -> &V;
    fn get_at_mut(&mut self, key: &K, depth: usize) -> &mut V;
}

impl<K, V> GetAt<K, V> for Stack<HashMap<K, V>>
where
    K: Eq + std::hash::Hash + Display,
{
    fn get_at(&self, key: &K, depth: usize) -> &V {
        let scope_index = self.len() - 1 - depth;
        let scope = &self[scope_index];
        scope.get(key).expect(&format!(
            "No key `{key}` in the scope. \
        If you see this, it is probably compiler and resolver out of sync."
        ))
    }

    fn get_at_mut(&mut self, key: &K, depth: usize) -> &mut V {
        let scope_index = self.len() - 1 - depth;
        let scope = &mut self[scope_index];
        scope.get_mut(key).expect(&format!(
            "No key `{key}` in the scope. \
        If you see this, it is probably compiler and resolver out of sync."
        ))
    }
}
