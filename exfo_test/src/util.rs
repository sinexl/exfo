use std::fmt::Formatter;
use std::path::Path;
use std::{fmt, fs, io};

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

pub fn ensure_compiler_exists<P: AsRef<Path>>(compiler_path: P) -> io::Result<()> {
    let compiler_path = compiler_path.as_ref();
    ensure_exists!(compiler_path, "Could not find compiler", "Ensure that exfo was built & is located at {}", compiler_path.display())?;
    Ok(())
}

pub fn ensure_exists_fmt<P: AsRef<Path>>(path: P, failure: &str, hint: impl Display) -> io::Result<()> {
    let path = path.as_ref();
    if !path.exists() {
        eprintln!(
            "Error: {failure} at {}",
            path.to_str().ok_or(io::ErrorKind::NotFound)?,
        );
        eprintln!("{}", hint);
        Err(io::ErrorKind::NotFound)?;
    }
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

pub struct DisplayBox<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayBox<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(message) = self;
        let len = message.len() * 3 / 2;
        writeln!(f, "\t┌{}┐", "─".repeat(len))?;
        writeln!(f, "\t│{:^width$}│", message, width = len)?;
        writeln!(f, "\t└{}┘", "─".repeat(len))?;
        Ok(())
    }
}
