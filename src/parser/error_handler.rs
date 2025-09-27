use crate::parser::error::ParseError; // adjust path to where error.rs is in your crate

/// Report parse error in a standard format.
/// If you have line/column information available, use `report_with_pos`.
pub fn report(err: &ParseError) {
    eprintln!("Parse error: {}", err);
}

/// If you later attach position info to tokens, call this to include location.
pub fn report_with_pos(err: &ParseError, line: usize, column: usize) {
    eprintln!("Parse error at {}:{}: {}", line, column, err);
}

/// Convenience that prints and exits with a non-zero status.
pub fn report_and_exit(err: &ParseError) -> ! {
    report(err);
    std::process::exit(1);
}
