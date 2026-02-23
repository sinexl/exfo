mod util;
mod command;

use crate::command::Subcommand;
use crate::util::{ensure_compiler_exists, remove_dir_contents, DisplayBox};
use serde::{Deserialize, Serialize};
use similar::TextDiff;
use std::collections::BTreeMap;
use std::env::Args;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::{cmp, env, fs, io};

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, PartialEq, Eq, Clone)]
pub enum TestResult {
    CompilerFailure,
    SuccessfulExecution { stdout: String, return_code: i32 },
}

impl TestResult {
    pub fn descriptive(&self) -> &'static str {
        match self {
            TestResult::CompilerFailure => "test compilation failure",
            TestResult::SuccessfulExecution { .. } => "successful test execution",
        }
    }
}


pub const RESET: &str = "\x1b[0m";
pub const GREEN: &str = "\x1b[32m";
pub const YELLOW: &str = "\x1b[33m";
pub const GREY: &str = "\x1b[90m";
pub const RED: &str = "\x1b[31m";
pub const BLUE: &str = "\x1b[94m";

type TestResults = BTreeMap<String, TestResult>;

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
    let command = Subcommand::parse(&mut args);

    let mut tests: Vec<String> = Vec::new();
    let dir = fs::read_dir(test_folder)?;
    // Iterate only over files
    for e in dir {
        let Ok(e) = e else {
            continue;
        };
        if e.path().extension() == Some(OsStr::new("exfo")) {
            let path = e.path();
            let test_case = path
                .file_stem()
                .ok_or(io::ErrorKind::NotFound)?
                .to_string_lossy()
                .into_owned();
            tests.push(test_case);
        }
    }

    ensure_exists!(json_path, "Could not find recorded test results", "Use `{program_name}` to record tests first")?;

    let recorded = fs::read_to_string(json_path)?;
    let recorded: TestResults = serde_json::from_str(&recorded)?;
    match &command {
        Subcommand::Record => {
            println!("Recording tests...");
            let results = record_tests(compiler_path, &tests, test_folder, test_bin)?;
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
            let got: TestResults = record_tests(compiler_path, &tests, test_folder, test_bin)?;
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
    tests: &[String],
    test_folder: P,
    test_bin: P,
) -> io::Result<TestResults> {
    let compiler_path = compiler_path.as_ref();
    let test_bin = test_bin.as_ref();
    let test_folder = test_folder.as_ref();

    let mut results: TestResults = BTreeMap::new();
    for test in tests {
        let test_path = test_folder.join(test).with_extension("exfo");
        let test_output = test_bin.join(test);
        let mut compiler = Command::new(compiler_path)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .arg(&test_path)
            .arg("-o")
            .arg(&test_output)
            .spawn()?;

        if !compiler.wait()?.success() {
            results.insert(test.clone(), TestResult::CompilerFailure);
            continue;
        }

        let output = Command::new(&test_output).output()?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let return_code = output.status.code().ok_or(io::ErrorKind::NotFound)?;

        results.insert(
            test.clone(),
            TestResult::SuccessfulExecution {
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
        let total_tab = " ".repeat(max_name_length + 1);
        print!("{case}:{tab}");
        let expected = recorded.get(case).unwrap();
        let got = got.get(case).unwrap();

        use TestResult::*;
        match (expected, got) {
            // wow rust pattern matching so cool!
            (a, b) if a == b => println!("{GREEN}OK{RESET}"),
            // wow rust pattern matching so cool! Just as copying the same match arm & this message.
            (CompilerFailure, CompilerFailure) => println!("{GREEN}OK{RESET}"),

            (
                SuccessfulExecution {
                    stdout: expected_stdout,
                    return_code: expected_code,
                },
                SuccessfulExecution {
                    stdout: got_stdout,
                    return_code: got_code,
                },
            ) => {
                if failed == 0 {
                    remove_dir_contents(diff_folder)?;
                }
                failed += 1;
                if expected_code != got_code {
                    println!("{RED}ERR{RESET}: RETURN CODES DIFFER:");
                    println!(
                        "{total_tab}GOT({RED}{}{RESET}) != EXPECTED({GREEN}{}{RESET})",
                        expected_code, got_code,
                    );
                }
                if expected_stdout != got_stdout {
                    println!("{RED}ERR{RESET}: OUTPUTS DIFFER");
                    let path = diff_folder.join(case).with_extension("diff");
                    {
                        let lines =
                            cmp::max(expected_stdout.lines().count(), got_stdout.lines().count());
                        let diff = TextDiff::from_lines(expected_stdout, got_stdout)
                            .unified_diff()
                            .context_radius(lines)
                            .header("EXPECTED STDOUT", "GOT STDOUT")
                            .to_string();
                        let mut file = OpenOptions::new().create(true).write(true).open(&path)?;
                        file.write_all(diff.as_bytes())?;
                    }
                    // Put :0:0 because some editors/terminals only recognize <filepath>:<line>:<column>-kind-of format
                    println!("{total_tab}at {}:1:1", path.display());
                }
            }
            // the last case left: if test was expected to fail, but it executed successfully or vice versa.
            (expected, got) => {
                failed += 1;  
                println!("{RED}ERR{RESET}:");
                println!(
                    "{total_tab}expected {} but got {}",
                    expected.descriptive(),
                    got.descriptive()
                );
            }
        };
    }
    Ok(failed)
}
