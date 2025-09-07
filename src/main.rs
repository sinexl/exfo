use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::code_generation::codegen::Codegen;
use crate::common::CompilerError;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::path::Path;
use std::process::{exit, Command, Stdio};
use std::{env, fs, io};

mod ast;
mod code_generation;
pub mod common;
mod compiling;
pub mod lexing;
mod parsing;
mod analysis;

fn get_line(msg: &str) -> String {
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
fn dev_repl() {
    let mut exit = false;
    while !exit {
        let ast_alloc = Bump::new();
        let ir_alloc = Bump::new();
        let mut compilation_errors: Vec<&dyn CompilerError> = vec![];
        let input = get_line("> ");

        if input == ":quit" {
            exit = true;
        }

        let (tokens, errors) = Lexer::new(&input, "<REPL>").accumulate();
        compilation_errors.reserve(errors.len());
        errors
            .iter()
            .for_each(|e| compilation_errors.push(e as &dyn CompilerError));

        let mut parser = Parser::new(tokens.into(), &ast_alloc);
        let (statements, errors) = parser.parse_program();
        errors
            .iter()
            .for_each(|e| compilation_errors.push(e as &dyn CompilerError));

        let mut comp = Compiler::new(&ir_alloc);
        comp.compile_statements(&statements);

        if compilation_errors.is_empty() {
            for statement in statements {
                println!("{}", statement);
            }
            for statement in statements {
                println!("{}", PrefixPrintStatement(statement));
            }

            println!("{ir}", ir = comp.ir);
        } else {
            for e in compilation_errors {
                println!("{e}");
            }
        }
    }
}

fn create_dir_if_not_exists<P: AsRef<Path>>(path: P) -> io::Result<()> {
    match fs::create_dir(path) {
        Ok(_) => {}
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(e),
    }
    Ok(())
}

struct DisplayCommand<'a>(&'a Command);

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
            eprintln!("{}", if_run_failed, );
            exit(1);
        }
    }
}

fn main() -> io::Result<()> {
    if env::args().next_back() == Some("--repl".to_owned()) {
        dev_repl();
        return Ok(());
    }
    let path = if let Some(filename) = env::args().nth(1) {
        filename
    } else {
        return Ok(());
    };
    let path = Path::new(&path);

    let file = fs::read_to_string(&path)?;

    // Compilation process.
    let mut compilation_errors: Vec<&dyn CompilerError> = vec![];
    let ast_allocator = Bump::new();
    let ir_allocator = Bump::new();
    // Lexing.
    let (tokens, errors) = Lexer::new(&file, path.to_str().unwrap()).accumulate();
    errors
        .iter()
        .for_each(|e| compilation_errors.push(e as &dyn CompilerError));
    // Parsing
    let mut parser = Parser::new(tokens.into(), &ast_allocator);
    let (ast, errors) = parser.parse_program();
    errors
        .iter()
        .for_each(|e| compilation_errors.push(e as &dyn CompilerError));
    // Error reporting
    if !compilation_errors.is_empty() {
        for e in compilation_errors {
            println!("{e}");
        }
        exit(1);
    }

    // Compilation to IR.
    let mut compiler = Compiler::new(&ir_allocator);
    compiler.compile_statements(ast);
    let ir = compiler.ir;
    println!("{ir}");

    let codegen = Codegen::new(ir);
    let generated_assembly = codegen.generate();
    println!("{}", generated_assembly);

    // Outputting the result of compilation to user.
    const BUILD_DIR: &str = "./.exfo_build";

    create_dir_if_not_exists(BUILD_DIR)?;
    let file_name = path.file_stem().and_then(|s| s.to_str()).unwrap();
    let asm_path = format!("{BUILD_DIR}/{file_name}.s");
    let asm_path = asm_path.as_str();
    fs::write(asm_path, generated_assembly.as_bytes())?;

    let object_path = format!("{BUILD_DIR}/{file_name}.o");
    let object_path = object_path.as_str();

    let mut gas = Command::new("as");
    gas.arg(asm_path)
        .arg("-o")
        .arg(object_path)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    println!("- Running assembler:\n{gas}", gas = DisplayCommand(&gas));
    run_command(
        &mut gas,
        "Assembler error occurred. Exiting.",
        "Failed to run `as` command. \n\t\
                     Make sure that you have installed GNU Assembler and it's available in $PATH",
    );

    let mut cc = Command::new("cc");
    cc.arg(object_path)
        .arg("src/putnum.c")
        .arg("-o")
        .arg(file_name)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    println!("- Running cc:\n{cc}", cc = DisplayCommand(&cc));
    run_command(
        &mut cc,
        "Failed to run cc. Exiting.",
        "Failed to run `cc` command. \n\tMake sure that `cc` is available in $PATH",
    );

    Ok(())
}
