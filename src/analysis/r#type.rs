use std::cell::Cell;
use crate::common::{BumpVec, Join};
use bumpalo::Bump;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

#[derive(PartialEq)]
pub enum Type<'types> {
    // Primitive Types
    // Naming convention:
    // Primitive - All types that are built in into the language.
    //   -> Basic: Types that are single-instance, meaning all values of a given
    //      basic type share the same TypeId (see BasicType for details).
    Basic(BasicType),
    Function(FunctionType<'types>),
    // UserDefined Types
    UserDefined(UserType<'types>),
}
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeId {
    Unknown,
    Index(usize),
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

#[derive(PartialEq)]
pub struct UserType<'types> {
    name: &'types str,
    size: usize,
}

impl<'types> UserType<'types> {
    pub fn new(types: &'types mut TypeCtx) -> Self {
        todo!("UserTypes are not implemented yet");
    }
}

pub struct TypeCtx<'types>(BumpVec<'types, Type<'types>>);

impl<'types> TypeCtx<'types> {
    pub fn new(types: &'types Bump) -> Self {
        let mut result = Self(BumpVec::with_capacity_in(BASIC_TYPES.len(), types));
        for basic_type in BASIC_TYPES {
            result.0.push(Type::Basic(*basic_type));
        }
        result
    }

    pub fn allocate(&self, item: Type<'types>) -> TypeId {
        let len = self.0.len();
        unsafe {
            let p = self as *const Self as *mut Self;
            (*p).0.push(item);
        }
        TypeId::Index(len)
    }

    pub fn bump(&self) -> &'types Bump {
        self.0.bump()
    }
}

impl<'types> Index<usize> for TypeCtx<'types> {
    type Output = Type<'types>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'types> IndexMut<usize> for TypeCtx<'types> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl TypeId {
    pub fn get<'types>(self, arena: &'types TypeCtx<'types>) -> &'types Type<'types> {
        match self {
            TypeId::Unknown => panic!("Cannot get unknown type"),
            TypeId::Index(id) => &arena[id],
        }
    }

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


    pub fn set(&self, value: TypeId) {
        unsafe {
            let cell_ptr = self as *const Self as *const Cell<Self>;
            (&*cell_ptr).set(value);
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub struct FunctionType<'types> {
    pub return_type: TypeId,
    pub parameters: &'types [TypeId],
    pub is_variadic: bool,
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
                            Join(fun.parameters.iter().map(|e| DisplayType(*e, types)), ", "),
                            DisplayType(fun.return_type, types)
                        )?;
                    }
                    Type::UserDefined(user_type) => {
                        todo!("User types are not implemented yet")
                    }
                }
            }
        }
        Ok(())
    }
}

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
