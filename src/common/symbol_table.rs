use crate::ast::expression::SymId;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::mem::zeroed;
use std::ops::{Index, IndexMut};

pub trait CompilerEntity: Debug {}

pub struct SymbolTable<T: CompilerEntity> {
    inner: Vec<T>,
}

impl<T: CompilerEntity> SymbolTable<T> {
    pub fn new(entity_amount: usize) -> Self {
        let mut s = Self {
            inner: Vec::with_capacity(entity_amount),
        };
        // s.inner.resize(entity_amount, Default::default());
        unsafe {
            s.inner.resize_with(entity_amount, || zeroed::<T>());
        }
        s
    }

    pub fn insert(&mut self, sym_id: SymId, value: T) {
        self[sym_id] = value
    }
}

impl<T: CompilerEntity> Index<SymId> for SymbolTable<T> {
    type Output = T;
    fn index(&self, index: SymId) -> &Self::Output {
        let SymId(idx) = index;
        &self.inner[idx]
    }
}

impl<T: CompilerEntity> IndexMut<SymId> for SymbolTable<T> {
    fn index_mut(&mut self, index: SymId) -> &mut Self::Output {
        let SymId(idx) = index;
        &mut self.inner[idx]
    }
}

impl<T: CompilerEntity> Debug for SymbolTable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(
                self.inner
                    .iter()
                    .enumerate()
                    .map(|(i, e)| format!("{i} => {e:?}")),
            )
            .finish()
    }
}
