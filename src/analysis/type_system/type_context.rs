use crate::analysis::r#type::{DisplayType, PointerType, Type, TypeId, TypeInner, BASIC_TYPES};
use crate::common::BumpVec;
use bumpalo::Bump;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};
use std::ptr::addr_of_mut;

pub struct TypeCtx<'types> {
    types: BumpVec<'types, Type<'types>>,
    // T -> T*
    pointer_monomorphisms: HashMap<PointerType, usize>,
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

    pub fn monomorph_or_get_pointer(&mut self, pointer_type: PointerType) -> TypeId {
        let pointer_monomorphisms = addr_of_mut!(self.pointer_monomorphisms);
        self.monomorph_or_get(pointer_monomorphisms, pointer_type)
    }

    pub fn monomorph_or_get<T>(
        &mut self,
        container: *mut HashMap<T, usize>,
        inner_type: T,
    ) -> TypeId
    where
        T: TypeInner<'types>,
    {
        /*
            Choosing between stronger type system (instead of stinky macros) and "safety", I would rather choose stronger type system.
            The reason for this unsafe is that this function requires both the mutable pointer to monomorphisms container AND &mut self to use self.allocate()
            Which is obviously not allowed in rust.
        */
        let container = unsafe {
            container
                .as_mut()
                .expect("COMPILER BUG: NULL TYPE CONTAINER PASSED")
        };
        let result = if container.contains_key(&inner_type) {
            *container.get(&inner_type).unwrap()
        } else {
            let TypeId::Index(type_id) = self.allocate(inner_type.to_type()) else {
                unreachable!("COMPILER BUG: TypeCtx::allocate returned `Unknown` Type")
            };
            container.insert(inner_type, type_id);
            type_id
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
