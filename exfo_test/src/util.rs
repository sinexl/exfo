use std::fmt::{Display, Formatter};
use std::path::Path;
use std::{fmt, fs, io};

pub mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const GREEN: &str = "\x1b[32m";
    pub const YELLOW: &str = "\x1b[33m";
    pub const GREY: &str = "\x1b[90m";
    pub const RED: &str = "\x1b[31m";
    pub const BLUE: &str = "\x1b[94m";
}


#[macro_export]
macro_rules! ensure_exists {
    ($path:expr, $failure:expr, $($fmt:tt)*) => {
        $crate::util::ensure_exists_fmt(
            $path,
            $failure,
            format!($($fmt)*),
        )
    };
}

#[macro_export]
macro_rules! warning {
    ($($arg:tt)*) => {{
        use $crate::util::colors::*;
        print!("{YELLOW}WARNING{RESET}: ");
        println!($($arg)*);
    }};
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {{
        use $crate::util::colors::*;
        eprint!("{RED}ERROR{RESET}: ");
        eprintln!($($arg)*);
    }};
}

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {{
        use $crate::util::colors::*;
        eprint!("{BLUE}INFO{RESET}: ");
        eprintln!($($arg)*);
    }};
}

pub fn ensure_exists_fmt<P: AsRef<Path>>(path: P, failure: &str, hint: impl Display) -> io::Result<()> {
    let path = path.as_ref();
    if !path.exists() {
        error!(
            "{failure} at {}",
            path.to_str().ok_or(io::ErrorKind::NotFound)?,
        );
        eprintln!("{}", hint);
        Err(io::ErrorKind::NotFound)?;
    }
    Ok(())
}


pub fn ensure_compiler_exists<P: AsRef<Path>>(compiler_path: P) -> io::Result<()> {
    let compiler_path = compiler_path.as_ref();
    ensure_exists!(compiler_path, "Could not find compiler", "Ensure that exfo was built & is located at {}", compiler_path.display())?;
    Ok(())
}


pub fn remove_dir_contents(path: impl AsRef<Path>) -> io::Result<()> {
    let path = path.as_ref();
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            fs::remove_dir_all(path)?;
        } else {
            fs::remove_file(path)?;
        }
    }
    Ok(())
}

pub struct DisplayBox<T>(pub T) where T: Display;

impl<T> Display for DisplayBox<T>  where T: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(message) = self;
        let message = message.to_string();
        let len = message.len() * 3 / 2;
        writeln!(f, "\t┌{}┐", "─".repeat(len))?;
        writeln!(f, "\t│{:^width$}│", message, width = len)?;
        writeln!(f, "\t└{}┘", "─".repeat(len))?;
        Ok(())
    }
}
