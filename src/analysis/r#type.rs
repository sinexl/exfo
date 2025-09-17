#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum Type {
    Unknown,
    Int64,
}

impl Type {
    pub fn size(self) -> usize { 
        match self {
            Type::Unknown => panic!(".size() should never be called on unknown"),
            Type::Int64 => 8,
        }
    }
}
