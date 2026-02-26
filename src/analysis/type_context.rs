use crate::analysis::r#type::{DisplayType, PointerType, Type, TypeId, BASIC_TYPES};
use crate::common::BumpVec;
use bumpalo::Bump;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};

pub struct TypeCtx<'types> {
    types: BumpVec<'types, Type<'types>>,
    // T -> T*
    pointer_monomorphisms: HashMap<usize, usize>,
}

impl<'types> Debug for TypeCtx<'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.types.iter().enumerate().map(|(i, _)| {
                let ty = TypeId::Index(i);
                format!("{i} => {}", DisplayType(ty, self))
            }))
            .finish()
    }
}

impl<'types> TypeCtx<'types> {
    pub fn new(types: &'types Bump) -> Self {
        let mut result = Self {
            types: BumpVec::with_capacity_in(BASIC_TYPES.len(), types),
            pointer_monomorphisms: HashMap::new(),
        };
        for basic_type in BASIC_TYPES {
            result.types.push(Type::Basic(*basic_type));
        }
        result
    }

    pub fn allocate(&mut self, item: Type<'types>) -> TypeId {
        let len = self.types.len();
        self.types.push(item);
        TypeId::Index(len)
    }

    pub fn monomorph_or_get_pointer(&mut self, id: TypeId) -> TypeId {
        let TypeId::Index(id) = id else {
            panic!("COMPILER BUG: Could not monomorph `Unknown` type")
        };
        let result = if self.pointer_monomorphisms.contains_key(&id) {
            *self.pointer_monomorphisms.get(&id).unwrap()
        } else {
            let TypeId::Index(pointer_id) = self.allocate(Type::Pointer(PointerType {
                inner: TypeId::Index(id),
            })) else {
                unreachable!("COMPILER BUG: TypeCtx::allocate returned `Unknown` Type")
            };
            self.pointer_monomorphisms.insert(id, pointer_id);
            pointer_id
        };

        TypeId::Index(result)
    }

    pub fn bump(&self) -> &'types Bump {
        self.types.bump()
    }
}

impl<'types> Index<usize> for TypeCtx<'types> {
    type Output = Type<'types>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.types[index]
    }
}

impl<'types> IndexMut<usize> for TypeCtx<'types> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.types[index]
    }
}
