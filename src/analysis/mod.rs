pub mod resolver;
#[cfg(test)]
mod tests;

#[path = "type_system/r#type.rs"]
pub mod r#type;
pub mod type_system;

type Stack<T> = Vec<T>;
