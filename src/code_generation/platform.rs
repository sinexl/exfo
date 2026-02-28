use crate::code_generation::x86_64;

#[allow(non_camel_case_types)]
pub enum Platform {
    x86_64(x86_64::Os)
}