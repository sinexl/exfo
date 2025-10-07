use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env::Args;
use std::path::Path;
use std::process::Command;
use std::{env, fs, io};

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, PartialEq, Eq)]
struct TestResult {
    stdout: String,
    return_code: i32,
}

#[derive(Debug, Clone)]
enum Subcommand {
    Record,
    Check,
}

pub const RESET: &str = "\x1b[0m";
pub const GREEN: &str = "\x1b[32m";
pub const YELLOW: &str = "\x1b[33m";
pub const GREY: &str = "\x1b[90m";
pub const RED: &str = "\x1b[31m";
pub const BLUE: &str = "\x1b[94m";

type TestResults = HashMap<String, TestResult>;

fn main() -> io::Result<()> {
    let json_path = Path::new("./src/tests.json");
    let test_folder = Path::new("./tests/");
    let test_bin = Path::new("./tests/bin/");
    let compiler_path = Path::new("./../target/debug/exfo");
    fs::create_dir_all(test_folder)?;
    fs::create_dir_all(test_bin)?;
    ensure_compiler_exists(compiler_path)?;

    let mut args = std::env::args();
    let program_name = args.next().unwrap();
    let command = parse_command(&mut args);

    let mut paths = Vec::new();
    let dir = fs::read_dir(test_folder)?;
    // Iterate only over files
    dir.filter_map(Result::ok)
        .filter(|e| {
            e.path()
                .extension()
                .map(|ext| ext == "exfo")
                .unwrap_or(false)
        })
        .for_each(|e| {
            paths.push(e.path());
        });

    match &command {
        Subcommand::Record => {
            println!("Recording tests...");
            let results = record_tests(compiler_path, &paths, test_bin)?;
            let json = serde_json::to_string(&results)?;
            fs::write(json_path, json)?;
            println!(
                "Recorded {n} test case(s) into {path}",
                n = results.len(),
                path = json_path.display()
            )
        }
        Subcommand::Check => {
            if !json_path.exists() {
                eprintln!(
                    "Error: Could not find recorded test results at: {json_path}",
                    json_path = json_path.display()
                );
                eprintln!("Use `{program_name} record` to record tests first");
                Err(io::ErrorKind::NotFound)?;
            }
            let json = fs::read_to_string(json_path)?;
            let recorded: TestResults = serde_json::from_str(&json)?;
            let got: TestResults = record_tests(compiler_path, &paths, test_bin)?;
            check_tests(&recorded, &got);
        }
    };

    Ok(())
}

fn record_tests<P: AsRef<Path>>(
    compiler_path: P,
    paths: &[impl AsRef<Path>],
    test_bin: P,
) -> io::Result<TestResults> {
    let compiler_path = compiler_path.as_ref();
    let test_bin = test_bin.as_ref();
    let mut results: TestResults = HashMap::new();
    for path in paths {
        let path = path.as_ref();
        let stem = path.file_stem().unwrap().to_str().unwrap();
        let test_bin_path = test_bin.join(stem);
        println!("{}", env::current_dir()?.display()); 
        let helper_path = Path::new("./putnum.c");
        dbg!(helper_path.exists());
        Command::new(compiler_path)
            .arg(path)
            .arg("-o")
            .arg("-c_helper")
            .arg(helper_path)
            .arg(&test_bin_path)
            .output()?;

        let output = Command::new(&test_bin_path).output()?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let return_code = output.status.code().unwrap();

        results.insert(
            stem.to_string(),
            TestResult {
                stdout,
                return_code,
            },
        );
    }
    Ok(results)
}

fn check_tests(recorded: &TestResults, got: &TestResults) {
    if recorded.len() != got.len() {
        println!(
            "{YELLOW}WARNING{RESET}: found {a} testcases than recorded\n\tRecorded: {}\n\tGot: {}",
            recorded.len(),
            got.len(),
            a = if got.len() > recorded.len() {
                "more"
            } else {
                "less"
            },
        )
    }

    let max = recorded.keys().map(|e| e.len()).max().unwrap() + 1;
    for case in recorded.keys() {
        let tab = " ".repeat(max - case.len());
        print!("{case}:{tab}");
        if recorded.get(case).unwrap() == got.get(case).unwrap() {
            println!("{GREEN}OK{RESET}");
        } else {
            println!("{RED}ERR{RESET}");
        }
    }
}

fn ensure_compiler_exists<P: AsRef<Path>>(compiler_path: P) -> io::Result<()> {
    let compiler_path = compiler_path.as_ref();
    if !compiler_path.exists() {
        eprintln!(
            "Could not find compiler at {}",
            compiler_path.to_str().unwrap(),
        );
        eprintln!("Ensure that exfo was built and is located at src/target/debug/");
        Err(io::ErrorKind::NotFound)?;
    }

    Ok(())
}
fn parse_command(args: &mut Args) -> Subcommand {
    let arg = args.next();
    if arg.is_none() {
        return Subcommand::Check;
    }
    let arg = arg.unwrap();

    match arg.as_ref() {
        "record" => Subcommand::Record,
        "check" => Subcommand::Check,
        _ => Subcommand::Check,
    }
}
