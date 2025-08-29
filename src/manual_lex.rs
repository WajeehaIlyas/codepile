use crate::token::Token;

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

            'a'..='z' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphabetic() {
                        ident.push(c);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }
                if ident == "fn" {
                    tokens.push(Token::Function);
                } else {
                    tokens.push(Token::Identifier(ident));
                }
            }

            '0'..='9' => {
                let mut num = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        chars.next();
                        column += 1;
                    } else {
                        break;
                    }
                }
                match num.parse::<i64>() {
                    Ok(n) => tokens.push(Token::IntLit(n)),
                    Err(_) => {
                        return Err(LexError {
                            message: format!("Invalid integer literal: {}", num),
                            line,
                            column,
                        });
                    }
                }
            }

            '(' => {
                chars.next();
                column += 1;
                tokens.push(Token::ParenL);
            }
            ')' => {
                chars.next();
                column += 1;
                tokens.push(Token::ParenR);
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
