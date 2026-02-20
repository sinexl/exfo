use crate::analysis::resolver::Resolver;
use crate::analysis::type_context::TypeCtx;
use crate::analysis::typechecker::Typechecker;
use crate::ast::tree_printer::DisplayStatement;
use crate::code_generation::codegen::Codegen;
use crate::common::CompilerError;
use crate::compiler_io::compiler_arguments::CompilerArguments;
use crate::compiler_io::dev_repl::dev_repl;
use crate::compiler_io::util::{create_dir_if_not_exists, run_command, DisplayCommand};
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use bumpalo::Bump;
use std::path::Path;
use std::process::{exit, Command, Stdio};
use std::ptr::addr_of_mut;
use std::{fs, io};

mod analysis;
mod ast;
mod code_generation;
pub mod common;
mod compiler_io;
mod compiling;
pub mod lexing;
mod parsing;

fn main() -> io::Result<()> {
    let args = std::env::args();
    let args = CompilerArguments::parse(args);
    if args.repl {
        dev_repl();
        return Ok(());
    }

    let input = Path::new(&args.input);
    let output = args.output_file();
    let file = fs::read_to_string(input)?;

    // Compilation process.
    let mut static_errors: Vec<Box<dyn CompilerError>> = vec![];
    let ast_allocator = Bump::new();
    let type_allocator = Bump::new();
    let mut types = TypeCtx::new(&type_allocator);
    let types_ptr = addr_of_mut!(types);
    let ir_allocator = Bump::new();
    // Lexing.
    let (tokens, errors) = Lexer::new(&file, input.to_str().unwrap()).accumulate();
    push_errors!(static_errors, errors);
    // Parsing
    let mut parser = Parser::new(tokens.into(), &ast_allocator, types_ptr);
    let (ast, errors) = parser.parse_program();
    push_errors!(static_errors, errors);

    // Static analysis
    let mut resolver = Resolver::new();
    let errors = resolver.resolve_statements(ast);
    for w in resolver.warnings {
        eprintln!("{}", w);
    }
    push_errors!(static_errors, errors);
    let resolutions = resolver.resolutions;

    // Error reporting
    // TODO: Currently, compiler exits if are any errors at resolution pass, which is not correct.
    //   Ideally, Resolver and Typechecker should produce dummy-results on error, as much as possible
    //   errors could be reported.
    if !static_errors.is_empty() {
        for e in static_errors {
            eprintln!("{e}");
        }
        exit(1);
    }

    let mut type_checker = Typechecker::new(types_ptr, resolutions);
    let errors = type_checker.typecheck_statements(ast);
    if let Err(r) = errors {
        push_errors!(static_errors, r);
    }

    // Error reporting
    if !static_errors.is_empty() {
        for e in static_errors {
            eprintln!("{e}");
        }
        exit(1);
    }

    // Compilation to IR.
    let mut compiler = Compiler::new(&ir_allocator, types_ptr, type_checker.resolutions);
    compiler.compile_statements(ast);
    let ir = compiler.ir;

    let codegen = Codegen::new(ir, args.pic());
    let generated_assembly = codegen.generate();
    dprintln!(args, "{generated_assembly}");
    for i in ast {
        dprintln!(args, "{}", DisplayStatement(i, &types));
    }
    dprintln!(args, "{ir}");

    // Outputting the result of compilation to user.
    const BUILD_DIR: &str = "./.exfo_build";

    create_dir_if_not_exists(BUILD_DIR)?;
    let file_name = input.file_stem().and_then(|s| s.to_str()).unwrap();
    let asm_path = format!("{BUILD_DIR}/{file_name}.s");
    let asm_path = asm_path.as_str();
    fs::write(asm_path, generated_assembly.as_bytes())?;

    let object_path = format!("{BUILD_DIR}/{file_name}.o");
    let object_path = object_path.as_str();


    if let Some(parent) = output.parent()
        && parent.file_name().is_some_and(|e| e != "") // it's weird because if there is no parent directory, it returns "" which is considered non-existing
        && !parent.exists()
    {
        println!("Creating directory {parent}", parent = parent.display());
        fs::create_dir_all(parent).expect("failed to create output directory");
    }

    let mut gas = Command::new("as");
    if args.debug_compiler {
        gas.arg("-g");
    }
    gas.arg(asm_path)
        .arg("-o")
        .arg(object_path)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    println!("Running assembler:\n{gas}", gas = DisplayCommand(&gas));
    run_command(
        &mut gas,
        "Assembler error occurred. Exiting.",
        "Failed to run `as` command. \n\t\
                     Make sure that you have installed GNU Assembler and it's available in $PATH",
    );

    println!();
    let mut cc = Command::new("cc");
    cc.arg(object_path)
        .arg("-o")
        .arg(output)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    if args.pic() {
        cc.arg("-fPIC");
    } else {
        cc.arg("-no-pie");
    }

    println!("Running cc:\n{cc}", cc = DisplayCommand(&cc));
    run_command(
        &mut cc,
        "Failed to run cc. Exiting.",
        "Failed to run `cc` command. \n\tMake sure that `cc` is available in $PATH",
    );

    Ok(())
}
