pub mod codegen;
mod register;

#[derive(Eq, PartialEq)]
pub enum Os { 
    Linux, 
    Windows,
}