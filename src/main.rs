use crate::analysis::resolver::Resolver;
use crate::analysis::type_system::typechecker::Typechecker;
use crate::ast::tree_printer::DisplayStatement;
use crate::common::static_errors::StaticErrors;
use crate::common::symbol_table::Transform;
use crate::compiling::compiler::Compiler;
use crate::lexing::lexer::Lexer;
use crate::parsing::parser::Parser;
use analysis::type_system::type_context::TypeCtx;
use bumpalo::Bump;
use code_generation::x86_64::codegen::Codegen;
use dev_repl::dev_repl;
use exfo::compiler_io::compiler_arguments::CompilerArguments;
use exfo::compiler_io::util::{create_dir_if_not_exists, run_command, DisplayCommand};
use exfo::dprintln;
use exfo::target::target::x86_64::Os;
use exfo::target::target::Target;
use std::path::{Path, PathBuf};
use std::process::{exit, Command, Stdio};
use std::ptr::addr_of_mut;
use std::{env, fs, io};

mod analysis;
mod ast;
mod code_generation;
pub mod common;
mod compiling;
pub mod dev_repl;
pub mod lexing;
mod parsing;



fn main() -> io::Result<()> {
    let args = env::args();
    let args = CompilerArguments::parse(args);
    if args.repl {
        dev_repl();
        return Ok(());
    }

    let input = Path::new(&args.input);
    let output = args.output_file();
    let file = fs::read_to_string(input)?;

    let ast_allocator = Bump::new();
    let type_allocator = Bump::new();
    let ir_allocator = Bump::new();
    let error_allocator = Bump::new();

    let mut types = TypeCtx::new(&type_allocator);
    let types_ptr = addr_of_mut!(types);

    let mut static_errors = StaticErrors::new(&error_allocator);

    // Compilation process.
    // Lexing.
    let (tokens, errors) = Lexer::new(&file, input.to_str().unwrap()).accumulate();
    static_errors.lexer(errors);
    // Parsing
    let mut parser = Parser::new(tokens.into(), &ast_allocator, &error_allocator, types_ptr);
    let (ast, errors) = parser.parse_program();
    static_errors.parser(errors);

    if static_errors.len() != 0 {
        let mut e = String::new();
        static_errors.print(&mut e, &types).map_err(|_| io::Error::from(io::ErrorKind::Other))?;
        eprintln!("{}", e);
        exit(-1);
    }

    // Static analysis
    let mut resolver = Resolver::new(&error_allocator);
    let errors = resolver.resolve_statements(ast);
    for w in resolver.warnings {
        eprintln!("{}", w);
    }
    static_errors.resolver(errors);
    let symbols_count = parser.count_symbols();

    // TODO: Currently, compiler exits if are any errors at resolution pass, which is not correct.
    //   Ideally, Resolver and Typechecker should produce dummy-results on error, as much as possible
    //   errors could be reported.
    // Error reporting
    if static_errors.len() != 0 {
        let mut e = String::new();
        static_errors.print(&mut e, &types).map_err(|_| io::Error::from(io::ErrorKind::Other))?;
        eprintln!("{}", e);
        exit(-1);
    }

    let mut type_checker = Typechecker::new(&type_allocator, &error_allocator, types_ptr, symbols_count);
    let errors = type_checker.typecheck_statements(ast);
    if let Err(r) = errors {
        static_errors.typechecker(r);
    }

    // Error reporting
    if static_errors.len() != 0 {
        let mut e = String::new();
        static_errors.print(&mut e, &types).map_err(|_| io::Error::from(io::ErrorKind::Other))?;
        eprintln!("{}", e);
        exit(-1);
    }

    let compiler_entities = type_checker.symbols.transform();

    // Compilation to IR.
    let mut compiler = Compiler::new(&ir_allocator, types_ptr, compiler_entities);
    compiler.compile_statements(ast);
    let ir = compiler.ir;

    let target = args.target();
    let os = match target {
        Target::x86_64(os) => os,
    };
    let (assembler, linker) = match target {
        Target::x86_64(Os::Linux) => (PathBuf::from("as"), PathBuf::from("cc")),
        Target::x86_64(Os::Windows) => (
            Path::new("as").with_extension(env::consts::EXE_EXTENSION),
            Path::new("x86_64-w64-mingw32-gcc").with_extension(env::consts::EXE_EXTENSION),
        ),
    };

    let codegen = Codegen::new(ir, os, args.pic());
    let generated_assembly = codegen.generate();
    dprintln!(args, "{generated_assembly}");
    dprintln!(args, "Types = {types:#?}");
    for i in ast {
        dprintln!(args, "{}", DisplayStatement(i, &types));
    }
    dprintln!(args, "{ir}");

    // Outputting the result of compilation to user.
    let build_dir = PathBuf::from(".exfo_build");
    create_dir_if_not_exists(&build_dir)?;
    let file_name = input.file_stem().and_then(|s| s.to_str()).unwrap();
    let asm_path = build_dir.join(file_name).with_extension("s");
    fs::write(&asm_path, generated_assembly.as_bytes())?;

    let object_path = build_dir.join(file_name).with_extension("o");

    if let Some(parent) = output.parent()
        && parent.file_name().is_some_and(|e| !e.is_empty()) // it's weird because if there is no parent directory, it returns "" which is considered non-existing
        && !parent.exists()
    {
        println!("Creating directory {parent}", parent = parent.display());
        fs::create_dir_all(parent).expect("failed to create output directory");
    }

    let mut gas = Command::new(&assembler);
    if args.debug_compiler {
        gas.arg("--gdwarf-2");
    }
    gas.arg(asm_path)
        .arg("-o")
        .arg(&object_path)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());

    println!("Running assembler:\n{gas}", gas = DisplayCommand(&gas));
    run_command(
        &mut gas,
        "Assembler error occurred. Exiting.",
        &format!(
            "Failed to run `{}` command. \n\t\
                     Make sure that you have installed GNU Assembler (MinGW in case of Windows) and it's available on your system in PATH",
            assembler.display()
        ),
    );

    println!();
    let mut cc = Command::new(&linker);
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

    println!("Running linker:\n{cc}", cc = DisplayCommand(&cc));
    run_command(
        &mut cc,
        "Failed to run linker. Exiting.",
        &format!(
            "Failed to run linker command. \n\tMake sure that `{}` is available in $PATH",
            linker.display()
        ),
    );

    Ok(())
}
