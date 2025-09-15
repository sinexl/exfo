use crate::common::Stack;
use std::collections::HashMap;
pub trait GetAt<K, V> {
    fn get_at(&self, key: &K, depth: usize) -> &V;
    fn get_at_mut(&mut self, key: &K, depth: usize) -> &mut V;
}

impl<K, V> GetAt<K, V> for Stack<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    fn get_at(&self, key: &K, depth: usize) -> &V {
        let scope_index = self.len() - 1 - depth;
        let scope = &self[scope_index];
        scope.get(key).unwrap()
    }

    fn get_at_mut(&mut self, key: &K, depth: usize) -> &mut V {
        let scope_index = self.len() - 1 - depth;
        let scope = &mut self[scope_index];
        scope.get_mut(key).unwrap()
    }
}
