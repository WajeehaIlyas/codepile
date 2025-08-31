use crate::token::Token;
use std::collections::HashMap;
use crate::utils::{is_identifier_start,is_identifier_part,parse_int,parse_float,unescape_char,
};

#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

pub fn lex_manually(input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1;
    let mut column = 1;

    // --- Keywords ---
    let keywords: HashMap<&str, Token> = [
        ("fn", Token::Function),
        ("int", Token::Int),
        ("float", Token::Float),
        ("string", Token::String),
        ("bool", Token::Bool),
        ("return", Token::Return),
        ("if", Token::If),
        ("else", Token::Else),
        ("while", Token::While),
        ("for", Token::For),
        ("true", Token::True),
        ("false", Token::False),
    ]
    .into_iter()
    .collect();

    // --- Single-char tokens ---
    let single_tokens: HashMap<char, Token> = [
        ('*', Token::Mul),
        ('%', Token::Mod),
        ('^', Token::BitXor),
        ('~', Token::BitNot),
        ('(', Token::ParenL),
        (')', Token::ParenR),
        ('{', Token::BraceL),
        ('}', Token::BraceR),
        ('[', Token::BracketL),
        (']', Token::BracketR),
        (',', Token::Comma),
        (';', Token::Semicolon),
        (':', Token::Colon),
    ]
    .into_iter()
    .collect();

    // --- Multi-char operators ---
    let multi_tokens: HashMap<&str, Token> = [
        ("==", Token::Equals),
        ("!=", Token::NotEquals),
        ("<=", Token::LessEq),
        (">=", Token::GreaterEq),
        ("++", Token::Increment),
        ("--", Token::Decrement),
        ("+=", Token::PlusAssign),
        ("-=", Token::MinusAssign),
        ("<<", Token::ShiftLeft),
        (">>", Token::ShiftRight),
        ("&&", Token::And),
        ("||", Token::Or),
    ]
    .into_iter()
    .collect();

    while let Some(&ch) = chars.peek() {
        match ch {
            // Whitespace
            ' ' | '\t' => {
                chars.next();
                column += 1;
            }
            '\n' => {
                chars.next();
                line += 1;
                column = 1;
            }

            // --- Comments ---
            '/' => {
                chars.next();
                column += 1;
                if let Some(&next_ch) = chars.peek() {
                    if next_ch == '/' {
                        // single-line
                        while let Some(c) = chars.next() {
                            column += 1;
                            if c == '\n' {
                                line += 1;
                                column = 1;
                                break;
                            }
                        }
                        continue;
                    } else if next_ch == '*' {
                        // multi-line
                        chars.next();
                        column += 1;
                        let mut prev = '\0';
                        while let Some(c) = chars.next() {
                            column += 1;
                            if c == '\n' {
                                line += 1;
                                column = 1;
                            }
                            if prev == '*' && c == '/' {
                                break;
                            }
                            prev = c;
                        }
                        continue;
                    }
                }
                tokens.push(Token::Div);
            }

            // --- Identifiers & keywords ---
            _ if is_identifier_start(ch) => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if is_identifier_part(c) {
                        ident.push(c);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }
                if let Some(token) = keywords.get(ident.as_str()) {
                    tokens.push(token.clone());
                } else {
                    tokens.push(Token::Identifier(ident));
                }
            }

            // --- Numbers ---
            '0'..='9' => {
                let start_column = column;
                let mut num_str = String::new();
                let mut is_float = false;

                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        num_str.push(c);
                        chars.next();
                        column += 1;
                    } else if c == '.' && !is_float {
                        is_float = true;
                        num_str.push(c);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }

                if let Some(&next_ch) = chars.peek() {
                    if is_identifier_start(next_ch) {
                        return Err(LexError {
                            message: format!(
                                "Invalid identifier starting with number: {}{}",
                                num_str, next_ch
                            ),
                            line,
                            column: start_column,
                        });
                    }
                }

                if is_float {
                    match parse_float(&num_str) {
                        Ok(f) => tokens.push(Token::FloatLit(f)),
                        Err(msg) => {
                            return Err(LexError { message: msg, line, column: start_column })
                        }
                    }
                } else {
                    match parse_int(&num_str) {
                        Ok(i) => tokens.push(Token::IntLit(i)),
                        Err(msg) => {
                            return Err(LexError { message: msg, line, column: start_column })
                        }
                    }
                }
            }

            // --- String literals ---
            '"' => {
                chars.next();
                column += 1;
                let mut str_lit = String::new();
                while let Some(c) = chars.next() {
                    column += 1;
                    if c == '"' {
                        break;
                    } else if c == '\\' {
                        if let Some(esc) = chars.next() {
                            column += 1;
                            match unescape_char(esc) {
                                Ok(ch) => str_lit.push(ch),
                                Err(msg) => {
                                    return Err(LexError { message: msg, line, column });
                                }
                            }
                        } else {
                            return Err(LexError {
                                message: "Unterminated escape sequence".to_string(),
                                line,
                                column,
                            });
                        }
                    } else {
                        str_lit.push(c);
                    }
                }
                tokens.push(Token::StringLit(str_lit));
            }

            // --- Operators (multi-char first, fallback single-char) ---
            '=' | '!' | '<' | '>' | '+' | '-' | '&' | '|' => {
                let mut op = String::new();
                op.push(ch);
                chars.next();
                column += 1;

                if let Some(&next_ch) = chars.peek() {
                    op.push(next_ch);
                    if let Some(tok) = multi_tokens.get(op.as_str()) {
                        chars.next();
                        column += 1;
                        tokens.push(tok.clone());
                        continue;
                    }
                }

                // fallback single operator
                let tok = match ch {
                    '=' => Token::Assign,
                    '!' => Token::Not,
                    '<' => Token::Less,
                    '>' => Token::Greater,
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '&' => Token::BitAnd,
                    '|' => Token::BitOr,
                    _ => unreachable!(),
                };
                tokens.push(tok);
            }

            // --- Single-char tokens ---
            _ if single_tokens.contains_key(&ch) => {
                chars.next();
                column += 1;
                tokens.push(single_tokens[&ch].clone());
            }

            _ => {
                return Err(LexError {
                    message: format!("Unexpected character: {}", ch),
                    line,
                    column,
                });
            }
        }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
}
