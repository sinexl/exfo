use std::env;
use std::fmt::{Display, Formatter};

pub mod x86_64 {
    #[derive(Eq, PartialEq, Clone, Copy, Debug)]
    pub enum Os {
        Linux,
        Windows,
    }
}
#[allow(non_camel_case_types)]
#[derive(Copy, Eq, PartialEq, Clone, Debug)]
pub enum Target {
    x86_64(x86_64::Os),
}

impl Target {
    pub fn get_current_target() -> Option<Self> {
        match (env::consts::ARCH, env::consts::OS) {
            ("x86_64", "linux") => Some(Target::x86_64(x86_64::Os::Linux)),
            ("x86_64", "windows") => Some(Target::x86_64(x86_64::Os::Windows)),
            _ => None,
        }
    }
    pub fn name(self) -> &'static str {
        match self {
            Target::x86_64(x86_64::Os::Linux) => "x86_64-linux",
            Target::x86_64(x86_64::Os::Windows) => "x86_64-windows",
        }
    }

    pub fn exe_extension(self) -> &'static str {
        match self {
            Target::x86_64(x86_64::Os::Windows) => "exe",
            Target::x86_64(x86_64::Os::Linux) => "",
        }
    }

    pub fn x86_64_linux() -> Self {
        Self::x86_64(x86_64::Os::Linux)
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.name())
    }
}

pub const TARGETS: &[Target] = &[
    Target::x86_64(x86_64::Os::Linux),
    Target::x86_64(x86_64::Os::Windows),
];

impl TryFrom<&str> for Target {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "x86_64-linux" => Ok(Target::x86_64(x86_64::Os::Linux)),
            "x86_64-windows" => Ok(Target::x86_64(x86_64::Os::Windows)),
            _ => Err(()),
        }
    }
}
