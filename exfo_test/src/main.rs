mod util;

use crate::util::{DisplayBox, ensure_compiler_exists, remove_dir_contents};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env::Args;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::{env, fs, io};

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, PartialEq, Eq, Clone)]
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
    let diff_folder = Path::new("./tests/diff");
    let compiler_path = Path::new("./../target/debug/exfo");
    fs::create_dir_all(test_folder)?;
    fs::create_dir_all(test_bin)?;
    fs::create_dir_all(diff_folder)?;
    ensure_compiler_exists(compiler_path)?;

    let mut args = env::args();
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

            let message = format!(
                "Recorded {n} test case(s) into {path}",
                n = results.len(),
                path = json_path.display()
            );
            print!("{message}", message = DisplayBox(&message));
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
            let failed = check_tests(&recorded, &got, &diff_folder)?;

            if failed == 0 {
                remove_dir_contents(diff_folder)?;
            }

            let message = if failed != 0 {
                format!("{failed} test(s) failed...")
            } else {
                "All tests succeeded...".to_string()
            };
            print!("{message}", message = DisplayBox(&message));
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
        Command::new(compiler_path)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .arg(path)
            .arg("-o")
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

// Returns how much tests failed
fn check_tests(
    recorded: &TestResults,
    got: &TestResults,
    diff_folder: impl AsRef<Path>,
) -> io::Result<usize> {
    let diff_folder = diff_folder.as_ref();
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

    let max_name_length = recorded.keys().map(|e| e.len()).max().unwrap() + 1;
    let mut failed = 0;
    for case in recorded.keys() {
        let tab = " ".repeat(max_name_length - case.len());
        print!("{case}:{tab}");
        let expected = recorded.get(case).unwrap();
        let got = got.get(case).unwrap();

        match (expected, got) {
            (a, b) if a == b => {
                println!("{GREEN}OK{RESET}");
            }

            (a, b) => {
                if failed == 0 {
                    remove_dir_contents(diff_folder)?;
                }
                failed += 1;
                if a.return_code != b.return_code {
                    println!("{RED}ERR{RESET}: RETURN CODES DIFFER:");
                    println!(
                        "{}GOT({RED}{}{RESET}) != EXPECTED({GREEN}{}{RESET})",
                        " ".repeat(max_name_length + 1),
                        expected.return_code,
                        got.return_code
                    );
                }
                if a.stdout != b.stdout {
                    println!("{RED}ERR{RESET}: OUTPUTS DIFFER");
                    let path = diff_folder.join(case).with_extension("diff");
                    {
                        let mut file = OpenOptions::new()
                            .create(true)
                            .write(true)
                            // TODO: Produce actual diff
                            .open(&path)?;
                        file.write_all("< ------- EXPECTED STDOUT: ------- >\n".as_bytes())?;
                        file.write_all(format!("{}\n", expected.stdout).as_bytes())?;
                        file.write_all("< ------- GOT STDOUT: ------- >\n".as_bytes())?;
                        file.write_all(format!("{}\n", got.stdout).as_bytes())?;
                    }
                    // Put :0:0 because some editors/terminals only recognize <filepath>:<line>:<column>-kind-of format
                    println!(
                        "{}at {}:1:1",
                        " ".repeat(max_name_length + 1),
                        path.display()
                    );
                }
            }
        };
    }
    Ok(failed)
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
