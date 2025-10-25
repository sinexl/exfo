pub mod resolver;
#[cfg(test)]
mod tests;

pub mod get_at;
#[path = "./r#type.rs"]
pub mod r#type;
pub mod typechecker;
