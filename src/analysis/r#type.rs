use crate::common::{BumpVec, Join};
use bumpalo::Bump;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

#[derive(PartialEq, Debug)]
pub enum Type<'types> {
    // Primitive Types
    // Naming convention:
    // Primitive - All types that are built in into the language.
    //   -> Basic: Types that are single-instance, meaning all values of a given
    //      basic type share the same TypeId (see BasicType for details).
    Basic(BasicType),
    Function(FunctionType<'types>),
    Pointer(PointerType),
    // UserDefined Types
    UserDefined(UserType<'types>),
}
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeId {
    Unknown,
    Index(usize),
}

impl From<TypeId> for TypeIdCell {
    fn from(value: TypeId) -> Self {
        Self {
            inner: Cell::new(value),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeIdCell {
    inner: Cell<TypeId>,
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum BasicType {
    Void,
    Int64,
    Float64,
    Bool,
    CharPtr,
}
const BASIC_TYPES: &[BasicType] = &[
    BasicType::Void,
    BasicType::Int64,
    BasicType::Float64,
    BasicType::Bool,
    BasicType::CharPtr,
];

#[derive(PartialEq, Debug)]
pub struct UserType<'types> {
    name: &'types str,
    size: usize,
}

impl<'types> UserType<'types> {
    pub fn new(types: &'types mut TypeCtx) -> Self {
        todo!("UserTypes are not implemented yet");
    }
}

pub struct TypeCtx<'types> {
    types: BumpVec<'types, Type<'types>>,
    // T -> T*
    pointer_monomorphisms: HashMap<usize, usize>,
}

impl<'types> Debug for TypeCtx<'types> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.types.len() {
            let ty = TypeId::Index(i);
            writeln!(f, "--------")?;
            writeln!(f, "{}", DisplayType(ty, self))?;
        }
        Ok(())
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
            })) else { unreachable!("COMPILER BUG: TypeCtx::allocate returned `Unknown` Type") };
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

impl TypeId {
    pub fn get<'types>(self, arena: &'types TypeCtx<'types>) -> &'types Type<'types> {
        match self {
            TypeId::Unknown => panic!("Cannot get unknown type"),
            TypeId::Index(id) => &arena[id],
        }
    }

    // todo: get rid of this method, or change it to work with pointers.
    pub fn get_mut<'types>(self, arena: &'types TypeCtx<'types>) -> &'types mut Type<'types> {
        match self {
            TypeId::Unknown => panic!("Cannot get unknown type"),
            TypeId::Index(id) => unsafe {
                let p = &arena[id];
                (p as *const Type as *mut Type<'types>)
                    .as_mut()
                    .unwrap_or_else(|| unreachable!())
            },
        }
    }

    pub fn from_basic(basic: BasicType) -> Self {
        for (i, basic_type) in BASIC_TYPES.iter().enumerate() {
            if *basic_type == basic {
                return TypeId::Index(i);
            }
        }
        unreachable!("COMPILER BUG: BASIC_TYPES do not contain all of hte basic types defined")
    }
}

impl TypeIdCell {
    pub fn inner(&self) -> TypeId {
        self.inner.get()
    }
    pub fn get<'types>(&self, arena: &'types TypeCtx<'types>) -> &'types Type<'types> {
        self.inner.get().get(arena)
    }

    pub fn get_mut<'types>(self, arena: &'types TypeCtx<'types>) -> &'types mut Type<'types> {
        self.inner.get().get_mut(arena)
    }

    pub fn from_basic(basic: BasicType) -> Self {
        Self {
            inner: Cell::from(TypeId::from_basic(basic)),
        }
    }

    pub fn set(&self, value: TypeId) {
        self.inner.set(value);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionType<'types> {
    pub return_type: TypeIdCell,
    pub parameters: &'types [TypeIdCell],
    pub is_variadic: bool,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PointerType {
    pub inner: TypeId,
}

pub struct DisplayType<'types>(pub TypeId, pub &'types TypeCtx<'types>);
impl<'ast> Display for DisplayType<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self(id, types) = self;
        match id {
            TypeId::Unknown => write!(f, "unknown")?,
            _ => {
                let ty = id.get(types);
                match ty {
                    Type::Basic(primitive) => match primitive {
                        BasicType::Void => write!(f, "void")?,
                        BasicType::Bool => write!(f, "bool")?,
                        BasicType::CharPtr => write!(f, "char_ptr")?,
                        BasicType::Int64 => write!(f, "int64")?,
                        BasicType::Float64 => write!(f, "float64")?,
                    },
                    Type::Function(fun) => {
                        write!(
                            f,
                            "func({}): {}",
                            Join(
                                fun.parameters.iter().map(|e| DisplayType(e.inner(), types)),
                                ", "
                            ),
                            DisplayType(fun.return_type.inner(), types)
                        )?;
                    }
                    Type::UserDefined(user_type) => {
                        todo!("User types are not implemented yet")
                    }
                    Type::Pointer(PointerType { inner }) => {
                        write!(f, "{}*", DisplayType(*inner, types))?;
                    }
                }
            }
        }
        Ok(())
    }
}

// @refactor: hardcoded word sizes
impl<'types> Type<'types> {
    // Note that types should know their size without depending on the TypeCtx. Thus, they should be able to calculate their size on initialization.
    pub fn size(&self) -> usize {
        match self {
            Type::Basic(primitive) => match primitive {
                BasicType::Void => 0,
                BasicType::Int64 => 8,
                BasicType::Float64 => 8,
                BasicType::Bool => 1,
                BasicType::CharPtr => 8,
            },
            Type::Function(_) => 8,
            Type::UserDefined(UserType { size, .. }) => *size,
            Type::Pointer(_) => 8,
        }
    }
    pub fn is_bool(&self) -> bool {
        if let Type::Basic(c) = self
            && *c == BasicType::Bool
        {
            return true;
        }
        false
    }
}
