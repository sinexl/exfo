pub mod resolver;
#[cfg(test)]
mod tests;

#[path = "./r#type.rs"]
pub mod r#type;
pub mod typechecker;
pub mod type_context;
type Stack<T> = Vec<T>;
