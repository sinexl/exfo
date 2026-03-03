use std::env::Args;

#[derive(Debug, Clone)]
pub enum Subcommand {
    Record { all: bool },
    Check { preferred_pic: bool },
}

impl Subcommand {
    pub fn parse(program_name: &str, args: &mut Args) -> Self {
        let mut command = Subcommand::Check {
            preferred_pic: true,
        };

        let args = args.collect::<Vec<_>>();
        let mut index = 0;

        while index < args.len() {
            let arg = &args[index];
            match arg.as_ref() {
                "--help" | "help" => {
                    println!("Usage: {program_name} <COMMAND> [OPTIONS]");
                    println!("Commands:");
                    println!("  record [all]     Record tests.");
                    println!("      all          Record all tests, overwriting the previous results. (NOT recommended unless you changed a test)");
                    println!("                   Otherwise, record only new tests added");
                    println!();
                    println!("  check [no-pic]   Run checks");
                    println!("      no-pic       Disable PIC generation for targets where it's applicable");
                    println!();
                    println!("  help             Show this help message");
                    println!("NOTE: If no command is provided, 'check' is used by default.");
                    std::process::exit(0);
                }
                "record" => {
                    index += 1;
                    match args.get(index) {
                        Some(ch) if ch == "all" => {
                            command = Subcommand::Record { all: true };
                        }
                        _ => {
                            command = Subcommand::Record { all: false };
                        }
                    }
                    index += 1;
                }
                "check" => {
                    match args.get(index + 1) {
                        None => {
                            command = Subcommand::Check {
                                preferred_pic: true,
                            };
                        }
                        Some(str) if str == "no-pic" => {
                            command = Subcommand::Check {
                                preferred_pic: false,
                            };
                        }
                        Some(str) => {
                            eprintln!("Unknown check option: {str}");
                            eprintln!(
                                "Unknown subcommand: {str}. Use {program_name} --help for list of all commands"
                            );
                            std::process::exit(1);
                        }
                    }
                    index = index + 2
                }
                str => {
                    eprintln!(
                        "Unknown subcommand: {str}. Use {program_name} --help for list of all commands"
                    );
                    std::process::exit(1);
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
}
