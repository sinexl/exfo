mod command;
mod util;

use crate::command::Subcommand;
use crate::util::colors::*;
use crate::util::{DisplayBox, ensure_compiler_exists, remove_dir_contents};
use serde::{Deserialize, Serialize};
use similar::TextDiff;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio, exit};
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

type TestResults = BTreeMap<String, TestResult>;

pub fn load_test_results_from_file(json_path: impl AsRef<Path>) -> io::Result<TestResults> {
    let json_path = json_path.as_ref();

    let json = match fs::read_to_string(json_path) {
        Ok(recorded) => recorded,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(TestResults::new()),
        Err(e) => return Err(e.into()),
    };
    let test_results: TestResults = serde_json::from_str(&json)?;
    Ok(test_results)
}

fn write_results(results: &TestResults, json_path: impl AsRef<Path>) -> io::Result<()> {
    let json = serde_json::to_string(&results)?;
    fs::write(json_path, json)?;
    Ok(())
}

// Returns names of the tests found in "test_folder"
pub fn scan_tests_from_folder(test_folder: impl AsRef<Path>) -> io::Result<BTreeSet<String>> {
    let mut result = BTreeSet::new();
    for e in fs::read_dir(test_folder)? {
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
            if !test_case.starts_with("__") {
                assert!(
                    result.insert(test_case),
                    "Each test case must be inserted uniquely"
                );
            }
        }
    }

    Ok(result)
}

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

    let all_found = scan_tests_from_folder(test_folder)?;

    if !command.is_record_all() {
        ensure_exists!(
            json_path,
            "Could not find recorded test results",
            "Use `{program_name} record all` to record tests first"
        )?;
    }

    let mut test_results = load_test_results_from_file(json_path)?;
    let recorded = test_results
        .keys()
        .map(|k| k.clone())
        .collect::<BTreeSet<_>>();

    // CASE: There are more tests in the json file then in the folder.
    if all_found.len() < recorded.len() {
        info!(
            "There are more tests recorded than found in the {}. Removing the tests",
            test_folder.display()
        );
        let set_difference = recorded.difference(&all_found);
        for i in set_difference {
            info!("\tREMOVING test case {BLUE}{i}{RESET}");
            test_results.remove(i).unwrap();
        }

        write_results(&test_results, json_path)?;
        exit(0);
    }

    assert!(recorded.len() <= all_found.len());
    let difference = all_found
        .difference(&recorded)
        .map(|e| e.clone())
        .collect::<BTreeSet<_>>();
    match &command {
        Subcommand::Record { all } => {
            let new = !*all;

            println!("Recording tests...");
            let to_record = if new { &difference } else { &all_found };

            let mut results = evaluate_tests(compiler_path, to_record, test_folder, test_bin)?;
            assert_eq!(results.len(), to_record.len());
            for test in &difference {
                let value = results.get(test).unwrap();
                print!("{}", DisplayBox(&format!("New test: {test}"), None));
                match value {
                    TestResult::CompilerFailure => println!("Compilation Failed"),
                    TestResult::SuccessfulExecution {
                        stdout,
                        return_code,
                    } => {
                        println!("< ─── {BLUE}Return code{RESET}: {return_code} ─── >");
                        println!("{stdout}");
                    }
                }
            }

            if new {
                test_results.extend(results);
                results = test_results;
            }

            write_results(&results, json_path)?;

            let len = to_record.len();
            let message = if new {
                format!("Saved {len} new test case(s) into {}", json_path.display())
            } else {
                format!("Recorded {len} test case(s) into {}", json_path.display())
            };

            print!("{message}", message = DisplayBox(&message, Some(BLUE)));
        }
        Subcommand::Check => {
            let got: TestResults = evaluate_tests(compiler_path, &recorded, test_folder, test_bin)?;
            let failed = check_tests(&test_results, &got, &diff_folder)?;

            if failed == 0 {
                remove_dir_contents(diff_folder)?;
            }
            let (message, color) = if failed != 0 {
                (format!("{failed} test(s) failed..."), RED)
            } else {
                ("All tests succeeded...".to_string(), GREEN)
            };
            print!("{message}", message = DisplayBox(&message, Some(color)));

            if recorded.len() < all_found.len() {
                let diff = difference.len();

                warning!(
                    "Found {diff} test{} which {} not recorded {:?}",
                    if diff == 1 { "" } else { "s" }, // Plural
                    if diff == 1 { "is" } else { "are" },
                    difference
                );
            }
        }
    };

    Ok(())
}

fn evaluate_tests<P: AsRef<Path>>(
    compiler_path: P,
    tests: &BTreeSet<String>,
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

                // needed for pretty-printing on same padding
                let mut tabulation = "";
                if expected_code != got_code {
                    err!("RETURN CODES DIFFER:");
                    tabulation = total_tab.as_str();
                    println!(
                        "{total_tab}{tabulation}GOT({RED}{got_code}{RESET}) != EXPECTED({GREEN}{expected_code}{RESET})",
                        tabulation = total_tab,
                    );
                }
                if expected_stdout != got_stdout {
                    print!("{tabulation}");
                    err!("OUTPUTS DIFFER");
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
                    // Put :1:1 because some editors/terminals only recognize <filepath>:<line>:<column>-kind-of format
                    println!("{total_tab}{tabulation}at {}:1:1", path.display());
                }
            }
            // the last case left: if test was expected to fail, but it executed successfully or vice versa.
            (expected, got) => {
                if failed == 0 {
                    remove_dir_contents(diff_folder)?;
                }
                failed += 1;
                err!("");
                println!(
                    "{total_tab}expected {GREEN}{}{RESET} but got {RED}{}{RESET}",
                    expected.descriptive(),
                    got.descriptive()
                );
            }
        };
    }
    Ok(failed)
}
