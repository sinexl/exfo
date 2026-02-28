use exfo::target::target::{Target, TARGETS};
use std::path::PathBuf;

pub struct CompilerArguments {
    pub input: PathBuf,

    /// Output file
    ///
    /// If omitted, defaults to the stem of the input file
    pub output: Option<PathBuf>,

    /// Start development REPL instead of compiling
    pub repl: bool,

    /// Output intermediate representations (IR & ASM) into stdout.
    pub debug_compiler: bool,

    /// Generate position-dependent code
    pub no_pic: bool,

    /// Compilation Target
    pub target: Option<Target>,
}
macro_rules! fatal {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        std::process::exit(1);
    }};
}

impl CompilerArguments {
    pub fn output_file(&self) -> PathBuf {
        match &self.output {
            Some(out) => out.clone(),
            None => {
                let inp = self.input.clone();
                inp.file_stem().map(PathBuf::from).unwrap()
            }
        }
    }

    fn default() -> CompilerArguments {
        let target = Target::get_current_target();
        CompilerArguments {
            input: PathBuf::new(),
            output: None,
            repl: false,
            debug_compiler: false,
            no_pic: false,
            target,
        }
    }

    pub fn parse(mut args: std::env::Args) -> CompilerArguments {
        let mut found_input = false;
        let mut result = CompilerArguments::default();
        let program_name = args.next().unwrap();

        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--output" | "-o" => {
                    let path = match args.next() {
                        Some(p) => p,
                        None => {
                            fatal!("error: no output path provided: {program_name} --help");
                        }
                    };

                    result.output = Some(PathBuf::from(path));
                }
                "--target" | "-t" => {
                    let Some(target) = args.next() else {
                        fatal!("error: no target target provided: {program_name} --help");
                    };
                    if target == "list" {
                        for i in TARGETS {
                            println!("\t{i}");
                        }
                        std::process::exit(0);
                    }
                    let target = match Target::try_from(target.as_str()) {
                        Ok(target) => target,
                        Err(()) => fatal!("error: unsupported target: {program_name} -t list"),
                    };
                    result.target = Some(target);
                }

                "--no-pic" => {
                    result.no_pic = true;
                }

                "--debug-compiler" => {
                    result.debug_compiler = true;
                }

                "--repl" => {
                    if found_input {
                        fatal!(
                            "error: --repl conflicts with normal compilation and cannot produce output"
                        );
                    }
                    result.repl = true;
                }

                "--help" => {
                    println!("Usage: {program_name} [options]");
                    println!("\t--help                         Display this message.");
                    println!("\t--output | -o <path>           Desired output file.");
                    println!("\t--target | -t <target>/list    Desired output target.");
                    println!("\t--repl                         Start development REPL instead of compiling.");
                    println!("\t--no-pic                       Generate position-dependent code.");
                    println!("\t--debug-compiler               Output intermediate representations (IR & ASM) into stdout."
                    );
                    std::process::exit(0);
                }

                input if !input.starts_with("--") && !input.starts_with("-") => {
                    if result.repl {
                        fatal!(
                            "error: --repl conflicts with normal compilation and cannot produce output"
                        );
                    }

                    result.input = PathBuf::from(input);
                    if !result.input.exists() {
                        fatal!("error: `{}`: file not found.", result.input.display());
                    } else {
                        found_input = true;
                    }
                }

                unknown_flag => {
                    fatal!("error: unknown flag: `{unknown_flag}`");
                }
            }
        }

        if !found_input && !result.repl {
            fatal!("error: no input file(s)");
        }

        if result.target.is_none() {
            if let Some(current) = Target::get_current_target() {
                result.target = Some(current);
            } else {
                use std::env::consts::*;
                fatal!(
                    "error: you are currently on non-supported target ({ARCH}, {OS}).\n\
                     Please use -t <target> or --target <target> flag to use a supported target.",
                )
            }
        }

        result
    }
    pub fn pic(&self) -> bool {
        !self.no_pic
    }

    pub fn target(&self)  -> Target {
        self.target.unwrap()
    }
}
