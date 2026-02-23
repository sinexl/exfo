use std::env::Args;

#[derive(Debug, Clone)]
pub enum Subcommand {
    Record { all: bool },
    Check,
}

impl Subcommand {
    pub fn parse(args: &mut Args) -> Self {
        let arg = args.next();
        if arg.is_none() {
            return Subcommand::Check;
        }
        let arg = arg.unwrap();

        match arg.as_ref() {
            "record" => match args.next() {
                Some(ch) if &ch == "all" => Subcommand::Record { all: true },
                _ => Subcommand::Record { all: false },
            },
            "check" => Subcommand::Check,
            _ => Subcommand::Check,
        }
    }
}

