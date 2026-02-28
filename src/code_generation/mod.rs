pub mod x86_64;
mod platform;

#[macro_export]
macro_rules! assert_same {
    ($msg:expr, $first:expr $(, $rest:expr)+ $(,)?) => {{
        let first = $first;
        $(
            assert!(
                $first == $rest,
                "COMPILER BUG: Codegen: {} ({} != {})",
                $msg,
                $first,
                $rest
            );
        )+
        first
    }};
}

#[macro_export]
macro_rules! asm {
    ($dst:expr, $($fmt:tt)*) => {
        writeln!($dst.output, $($fmt)*).unwrap()
    };
}

#[macro_export]
macro_rules! comment {
    ($dst:expr, $($fmt:tt)*) => {{
        write!($dst.output, "  // ").unwrap();
        writeln!($dst.output, $($fmt)*).unwrap();
    }};

    ($dst:expr) => {
        writeln!($dst.output).unwrap()
    };
}
