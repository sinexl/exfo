use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct CompilerArguments {
    #[arg(value_name = "INPUT", required = true, conflicts_with = "repl")]
    pub input: PathBuf,

    /// Output file
    ///
    /// If omitted, defaults to the stem of the input file
    #[arg(short = 'o', long = "output", conflicts_with = "repl")]
    pub output: Option<PathBuf>,

    /// Start development REPL instead of compiling
    #[arg(long)]
    pub repl: bool,
    
    /// Output intermediate representations (IR & ASM) into stdout.
    #[arg(long)]
    pub debug_compiler: bool,
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
}
