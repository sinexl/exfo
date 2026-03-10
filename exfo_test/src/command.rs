use exfo::fatal;
use exfo::target::target::{TARGETS, Target};
use std::env::Args;

#[derive(Debug, Clone)]
pub enum Subcommand {
    Record { all: bool },
    Check { preferred_pic: bool, target: Target },
}

impl Subcommand {
    pub fn parse(program_name: &str, args: &mut Args) -> Self {
        let current_target = if let Some(target) = Target::get_current_target() {
            target
        } else {
            fatal!("Unknown current target.");
        };
        let mut command = Subcommand::Check {
            preferred_pic: true,
            target: current_target,
        };

        let mut args = args.peekable();
        while let Some(arg) = args.peek() {
            match arg.as_ref() {
                "--help" | "help" => Self::print_help(program_name),
                "-t" => {
                    args.next();
                    if current_target != Target::x86_64_linux() {
                        fatal!("Cross-testing is only available on x86_64-linux.")
                    }

                    let current_choice = if let Subcommand::Check {
                        preferred_pic: _preferred_pic,
                        target,
                    } = &mut command
                    {
                        target
                    } else {
                        fatal!("error: '-t' option is only available for `check` subcommand");
                    };

                    let Some(target) = args.peek().cloned() else {
                        fatal!("error: no target provided: {program_name} --help");
                    };
                    args.next();

                    if target == "list" {
                        for i in TARGETS {
                            println!("\t{i}");
                        }
                        std::process::exit(0);
                    }

                    let target = match Target::try_from(target.as_str()) {
                        Ok(target) => target,
                        Err(()) => fatal!("error: unsupported target: `{program_name} -t list`"),
                    };

                    *current_choice = target;
                }
                "record" => match args.peek() {
                    Some(ch) if ch == "all" => {
                        args.next();
                        command = Subcommand::Record { all: true };
                    }
                    _ => {
                        command = Subcommand::Record { all: false };
                    }
                },
                "check" => {
                    args.next();
                    match args.peek() {
                        None => {
                            command = Subcommand::Check {
                                preferred_pic: true,
                                target: current_target,
                            };
                        }
                        Some(str) if str == "--no-pic" => {
                            args.next();
                            if let Subcommand::Check {
                                preferred_pic,
                                target: _target,
                            } = &mut command
                            {
                                *preferred_pic = false;
                            } else {
                                fatal!("`--no-pic` option is only applicable to `check` command");
                            }
                        }
                        _ => {}
                    }
                }
                str => {
                    fatal!(
                        "Unknown subcommand: `{str}`. Use `{program_name} help` for list of all commands"
                    );
                }
            }
        }

        command
    }

    pub fn is_record_all(&self) -> bool {
        match self {
            Subcommand::Record { all } => *all,
            _ => false,
        }
    }
    fn print_help(program_name: &str) -> ! {
        let linux = Target::x86_64_linux().to_string();
        println!("Usage: {program_name} <COMMAND> [OPTIONS]");
        println!("Commands:");
        println!("  record [all]     Record tests.");
        println!("      all          Record all the tests, overwriting the previous results");
        println!("                   (NOT recommended unless you changed a test)");
        println!("                   Otherwise, record only new tests added");
        println!();
        println!("  check [no-pic]   Run checks");
        println!("      --no-pic     Disable PIC for targets where it's applicable");
        println!("      -t <target>  Cross-testing. Only available on {linux}");
        println!();
        println!("  help             Show this help message");
        println!("NOTE: If no command is provided, 'check' is used by default.");
        std::process::exit(0);
    }
}
