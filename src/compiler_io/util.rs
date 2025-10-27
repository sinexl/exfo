use std::fmt::{Display, Formatter};
use std::io::Write;
use std::path::Path;
use std::process::{Command, exit};
use std::{fs, io};

pub fn get_line(msg: &str) -> String {
    let mut res = String::new();
    print!("{msg}");
    let _ = io::stdout().flush();
    io::stdin().read_line(&mut res).unwrap();
    if let Some('\n') = res.chars().next_back() {
        res.pop();
    };
    if let Some('\r') = res.chars().next_back() {
        res.pop();
    };
    res
}

pub fn create_dir_if_not_exists<P: AsRef<Path>>(path: P) -> io::Result<()> {
    match fs::create_dir(path) {
        Ok(_) => {}
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(e),
    }
    Ok(())
}

pub fn run_command(cmd: &mut Command, if_non0_exit: &str, if_run_failed: &str) {
    match cmd.status() {
        Ok(status) => {
            if !status.success() {
                eprintln!("{}", if_non0_exit);
                exit(1);
            }
        }
        Err(e) => {
            eprintln!("{e}");
            eprintln!("{}", if_run_failed,);
            exit(1);
        }
    }
}
pub struct DisplayCommand<'a>(pub &'a Command);

impl<'a> Display for DisplayCommand<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            self.0.get_program().to_string_lossy(),
            self.0
                .get_args()
                .map(|a| a.to_string_lossy())
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[macro_export]
macro_rules! dprintln {
    ($out:expr, $($arg:tt)*) => {{
        if $out.debug_compiler {
            println!($($arg)*);
        }
    }};
}
