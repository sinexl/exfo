use std::path::{PathBuf};

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
    /// Will be removed later. Provide a path for a .c file that will be linked with the final executable
    pub helper: PathBuf,
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
        CompilerArguments {
            input: PathBuf::new(),
            output: None,
            repl: false,
            debug_compiler: false,
            no_pic: false,
            helper: PathBuf::from("src/putnum.c"),
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
                            eprintln!("error: no output path provided: {program_name} --help");
                            std::process::exit(1);
                        }
                    };

                    result.output = Some(PathBuf::from(path));
                }
                "--helper" => {
                    let path = match args.next() {
                        Some(p) => p,
                        None => {
                            eprintln!("error: no helper path provided: {program_name} --help");
                            std::process::exit(1);
                        }
                    };

                    result.helper = PathBuf::from(path);
                }
                "--no-pic"  => {
                    result.no_pic = true;
                }

                "--debug-compiler"  => {
                    result.debug_compiler = true;
                }
                "--repl" => {
                    if found_input  {
                        eprintln!("error: --repl conflicts with normal compilation and cannot produce output")
                    }
                    result.repl = true;
                }
                "--help" => {
                    println!("Usage: {program_name} [options]");
                    println!("\t--help                Display this message.");
                    println!("\t--output | -o <path>  the desired output file.");
                    println!("\t--repl                Start development REPL instead of compiling.");
                    println!("\t--no-pic              Generate position-dependent code.");
                    println!("\t--debug-compiler      Output intermediate representations (IR & ASM) into stdout.");
                    println!("\t--helper              Will be removed later. Provide a path for a .c file that will be linked with the final executable.");
                    std::process::exit(0);
                }
                input => {
                    if result.repl  {
                        eprintln!("error: --repl conflicts with normal compilation and cannot produce output");
                        std::process::exit(-1);
                    }


                    result.input = PathBuf::from(input);
                    if !result.input.exists() {
                        eprintln!("error: file not found.");

                    } else {
                        found_input = true;
                    }
                }
            }
        }
        if !found_input && !result.repl {
            eprintln!("error: no input file(s)");
            std::process::exit(1);
        }
        result
    }
    pub fn pic(&self) -> bool {
        !self.no_pic
    }
}
