use std::env::Args;

#[derive(Debug, Clone)]
pub enum Subcommand {
    Record { all: bool },
    Check { preferred_pic: bool },
}

impl Subcommand {
    pub fn parse(program_name: &str, args: &mut Args) -> Self {
        let arg = args.next();

        let arg = match arg {
            Some(arg) => arg,
            None => {
                return Subcommand::Check {
                    preferred_pic: true,
                };
            }
        };

        match arg.as_ref() {
            "help" => {
                println!("Usage: {program_name} <COMMAND> [OPTIONS]");
                println!("Commands:");
                println!("  record [all]        Record tests.");
                println!(
                    "      all             Record all tests, overwriting the previous recorded results. (NOT recommended unless you changed a test)"
                );
                println!("                      Otherwise, record only new tests added");
                println!();
                println!("  check [no-pic]      Run checks");
                println!(
                    "      no-pic          Disable PIC generation for targets where it's applicable"
                );
                println!();
                println!("  help                Show this help message");
                println!("NOTE: If no command is provided, 'check' is used by default.");
                std::process::exit(0);
            }
            "record" => match args.next() {
                Some(ch) if &ch == "all" => Subcommand::Record { all: true },
                _ => Subcommand::Record { all: false },
            },
            "check" => match args.next() {
                None => Subcommand::Check {
                    preferred_pic: true,
                },
                Some(str) if str == "no-pic" => Subcommand::Check {
                    preferred_pic: false,
                },
                Some(str) => {
                    eprintln!("Unknown check option: {str}");
                    std::process::exit(1);
                }
            },
            _ => Subcommand::Check {
                preferred_pic: true,
            },
        }
    }

    pub fn is_record_all(&self) -> bool {
        match self {
            Subcommand::Record { all } => *all,
            _ => false,
        }
    }
}
