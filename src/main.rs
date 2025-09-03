use crate::ast::prefix_printer::PrefixPrintStatement;
use crate::code_generation::codegen::Codegen;
use crate::common::CompilerError;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
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
mod simple_interpreter;

fn get_line(msg: &str) -> String {
    let mut res = String::new();
    print!("{msg}");
    let _ = std::io::stdout().flush();
    std::io::stdin().read_line(&mut res).unwrap();
    if let Some('\n') = res.chars().next_back() {
        res.pop();
    };
    if let Some('\r') = res.chars().next_back() {
        res.pop();
    };
    res
}
fn repl() {
    let mut exit = false;
    while !exit {
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

        let bump = Bump::new();
        let mut parser = Parser::new(tokens.into(), &bump);
        let (statements, errors) = parser.parse_program();
        errors
            .iter()
            .for_each(|e| compilation_errors.push(e as &dyn CompilerError));

        if compilation_errors.is_empty() {
            for statement in statements {
                println!("{}", statement);
            }
            for statement in statements {
                println!("{}", PrefixPrintStatement(statement));
            }
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

fn main() -> io::Result<()> {
    if env::args().next_back() == Some("--repl".to_owned()) {
        repl();
        return Ok(());
    }
    let path = if let Some(filename) = env::args().nth(1) {
        filename
    } else {
        return Ok(());
    };

    let path = Path::new(&path);

    let file = fs::read_to_string(&path)?;

    let (tokens, errors) = Lexer::new(&file, path.to_str().unwrap()).accumulate();

    for token in &tokens {
        println!("{}", token);
    }
    for error in &errors {
        println!("{}", error)
    }
    let ast_alloc = Bump::new();
    let mut parser = Parser::new(tokens.into(), &ast_alloc);
    let (ast, errors) = parser.parse_program();
    for error in &errors {
        println!("{}", error as &dyn CompilerError);
    }
    assert_eq!(errors.len(), 0);

    for node in ast {
        println!("{}", node);
    }
    let ir_bump = Bump::new();
    let mut compiler = Compiler::new(&ir_bump);

    compiler.compile_statements(ast);
    let ir = compiler.ir;
    println!("{ir:#?}");

    let codegen = Codegen::new(ir);
    let generated_assembly = codegen.generate();
    println!("{}", generated_assembly);

    const BUILD_DIR: &str = "./.build";

    create_dir_if_not_exists(BUILD_DIR)?;
    let file_name = path.file_stem().and_then(|s| s.to_str()).unwrap();
    let asm_path = format!("{BUILD_DIR}/{file_name}.s");
    let asm_path = asm_path.as_str();
    fs::write(asm_path, generated_assembly.as_bytes())?;

    let object_path = format!("{BUILD_DIR}/{file_name}.o");
    let object_path = object_path.as_str();
    let gas = Command::new("as")
        .arg(asm_path)
        .arg("-o")
        .arg(object_path)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .expect(
            "Failed to run `as` command. \
                     Make sure that you have installed GNU Assembler and it's available in $PATH",
        );

    if !gas.success() {
        eprintln!("Assembler error occurred. Exiting.");
        exit(1);
    }


    let cc = Command::new("cc")
        .arg(object_path)
        .arg("-o")
        .arg(file_name)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .expect("Failed to run `cc` command. \
                      Make sure that `cc` is available in $PATH");


    Ok(())
}
