use crate::token::Token;
use crate::utils::compute_line_col;
use regex::Regex;

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

pub fn lex_with_regex(input: &str) -> Result<Vec<Token>, LexError> {
    // Each entry is: regex, action
    let token_specification: Vec<(Regex, Option<fn(&str) -> Token>)> = vec![
        // Skip whitespace & comments
        (Regex::new(r"//[^\n]*").unwrap(), None),
        (Regex::new(r"/\*[\s\S]*?\*/").unwrap(), None),
        (Regex::new(r"\s+").unwrap(), None),
        // Keywords
        (Regex::new(r"\bfn\b").unwrap(), Some(|_| Token::Function)),
        (Regex::new(r"\bint\b").unwrap(), Some(|_| Token::Int)),
        (Regex::new(r"\bfloat\b").unwrap(), Some(|_| Token::Float)),
        (Regex::new(r"\bstring\b").unwrap(), Some(|_| Token::String)),
        (Regex::new(r"\bbool\b").unwrap(), Some(|_| Token::Bool)),
        (Regex::new(r"\breturn\b").unwrap(), Some(|_| Token::Return)),
        (Regex::new(r"\bif\b").unwrap(), Some(|_| Token::If)),
        (Regex::new(r"\belse\b").unwrap(), Some(|_| Token::Else)),
        (Regex::new(r"\bwhile\b").unwrap(), Some(|_| Token::While)),
        (Regex::new(r"\bfor\b").unwrap(), Some(|_| Token::For)),
        (Regex::new(r"\btrue\b").unwrap(), Some(|_| Token::True)),
        (Regex::new(r"\bfalse\b").unwrap(), Some(|_| Token::False)),
        // Literals
        (
            Regex::new(r"\d+\.\d+").unwrap(),
            Some(|s| Token::FloatLit(s.parse::<f64>().unwrap())),
        ),
        (
            Regex::new(r"\d+").unwrap(),
            Some(|s| Token::IntLit(s.parse::<i64>().unwrap())),
        ),
        // Invalid identifiers (numbers followed by letters/underscores)
        (
            Regex::new(r"\d+[A-Za-z_]\w*").unwrap(),
            Some(|s| Token::Invalid(s.to_string())),
        ),
        // String literals with escapes
        (
            Regex::new(r#""(\\.|[^"\\])*""#).unwrap(),
            Some(|s| {
                let stripped = &s[1..s.len() - 1]; // remove quotes
                let unescaped = stripped
                    .replace(r#"\\\""#, "\"") // \" → "
                    .replace(r#"\n"#, "\n") // \n → newline
                    .replace(r#"\t"#, "\t") // \t → tab
                    .replace(r#"\r"#, "\r") // \r → carriage return
                    .replace(r#"\\\\"#, "\\");
                Token::StringLit(unescaped)
            }),
        ),
        // Unicode identifiers (letters, numbers, underscore, emojis, symbols)
        (
            Regex::new(r"[\p{L}_\p{So}][\p{L}\p{N}_\p{So}]*").unwrap(),
            Some(|s| Token::Identifier(s.to_string())),
        ),
        // Increment/Decrement
        (Regex::new(r"\+\+").unwrap(), Some(|_| Token::Increment)),
        (Regex::new(r"--").unwrap(), Some(|_| Token::Decrement)),
        // Compound Assignment
        (Regex::new(r"\+=").unwrap(), Some(|_| Token::PlusAssign)),
        (Regex::new(r"-=").unwrap(), Some(|_| Token::MinusAssign)),
        // Operators
        (Regex::new(r"==").unwrap(), Some(|_| Token::Equals)),
        (Regex::new(r"!=").unwrap(), Some(|_| Token::NotEquals)),
        (Regex::new(r"<=").unwrap(), Some(|_| Token::LessEq)),
        (Regex::new(r">=").unwrap(), Some(|_| Token::GreaterEq)),
        (Regex::new(r"&&").unwrap(), Some(|_| Token::And)),
        (Regex::new(r"\|\|").unwrap(), Some(|_| Token::Or)),
        (Regex::new(r"=").unwrap(), Some(|_| Token::Assign)),
        (Regex::new(r"<").unwrap(), Some(|_| Token::Less)),
        (Regex::new(r">").unwrap(), Some(|_| Token::Greater)),
        (Regex::new(r"\+").unwrap(), Some(|_| Token::Plus)),
        (Regex::new(r"-").unwrap(), Some(|_| Token::Minus)),
        (Regex::new(r"\*").unwrap(), Some(|_| Token::Mul)),
        (Regex::new(r"/").unwrap(), Some(|_| Token::Div)),
        (Regex::new(r"%").unwrap(), Some(|_| Token::Mod)),
        (Regex::new(r"!").unwrap(), Some(|_| Token::Not)),
        // Bitwise Operators
        (Regex::new(r"&").unwrap(), Some(|_| Token::BitAnd)),
        (Regex::new(r"\|").unwrap(), Some(|_| Token::BitOr)),
        (Regex::new(r"\^").unwrap(), Some(|_| Token::BitXor)),
        (Regex::new(r"~").unwrap(), Some(|_| Token::BitNot)),
        (Regex::new(r"<<").unwrap(), Some(|_| Token::ShiftLeft)),
        (Regex::new(r">>").unwrap(), Some(|_| Token::ShiftRight)),
        // Symbols
        (Regex::new(r"\(").unwrap(), Some(|_| Token::ParenL)),
        (Regex::new(r"\)").unwrap(), Some(|_| Token::ParenR)),
        (Regex::new(r"\{").unwrap(), Some(|_| Token::BraceL)),
        (Regex::new(r"\}").unwrap(), Some(|_| Token::BraceR)),
        (Regex::new(r"\[").unwrap(), Some(|_| Token::BracketL)),
        (Regex::new(r"\]").unwrap(), Some(|_| Token::BracketR)),
        (Regex::new(r",").unwrap(), Some(|_| Token::Comma)),
        (Regex::new(r";").unwrap(), Some(|_| Token::Semicolon)),
        (Regex::new(r":").unwrap(), Some(|_| Token::Colon)),
    ];

    let mut tokens = Vec::new();
    let mut remaining = input;
    let mut offset = 0; // track position in original input

    while !remaining.is_empty() {
        let mut best_match: Option<(usize, &Regex, &Option<fn(&str) -> Token>)> = None;

        for (re, action) in &token_specification {
            if let Some(mat) = re.find(remaining) {
                if mat.start() == 0 {
                    // pick the longest match
                    if best_match.is_none() || mat.end() > best_match.unwrap().0 {
                        best_match = Some((mat.end(), re, action));
                    }
                }
            }
        }

        if let Some((end, _re, action)) = best_match {
            let lexeme = &remaining[..end];
            if let Some(act) = action {
                let tok = act(lexeme);

                // If it's an invalid token return error immediately
                if let Token::Invalid(s) = tok {
                    let (line, col) = compute_line_col(input, offset);
                    return Err(LexError {
                        message: format!("Invalid identifier: {}", s),
                        line,
                        column: col,
                    });
                }

                tokens.push(tok);
            }
            offset += end;
            remaining = &remaining[end..];
        } else {
            let (line, col) = compute_line_col(input, offset);
            let invalid_fragment = &remaining[..remaining.len().min(20)];
            return Err(LexError {
                message: format!("Unexpected token starting with {:?}", invalid_fragment),
                line,
                column: col,
            });
        }
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}
