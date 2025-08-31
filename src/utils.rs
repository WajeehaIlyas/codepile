/// Compute line/column for error reporting
pub fn compute_line_col(input: &str, index: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, ch) in input.char_indices() {
        if i == index {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Check if a character can start an identifier
pub fn is_identifier_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_' || (ch >= '🦀' && ch <= '🦿')
}

/// Check if a character can be *part* of an identifier
pub fn is_identifier_part(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_' || (ch >= '🦀' && ch <= '🦿')
}

/// Parse an integer safely, return Result<i64, String>
pub fn parse_int(num_str: &str) -> Result<i64, String> {
    num_str.parse::<i64>()
        .map_err(|_| format!("Invalid integer literal: {}", num_str))
}

/// Parse a float safely, return Result<f64, String>
pub fn parse_float(num_str: &str) -> Result<f64, String> {
    num_str.parse::<f64>()
        .map_err(|_| format!("Invalid float literal: {}", num_str))
}

/// Handle escape sequences inside string literals
pub fn unescape_char(esc: char) -> Result<char, String> {
    match esc {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        'r' => Ok('\r'),
        '"' => Ok('"'),
        '\\' => Ok('\\'),
        other => Err(format!("Invalid escape sequence: \\{}", other)),
    }
}
