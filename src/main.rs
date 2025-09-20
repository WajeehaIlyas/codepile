mod manual_lex;
mod regex_lex;
mod token;
mod utils;

use manual_lex::lex_manually;
use regex_lex::lex_with_regex;
use token::Token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} <lexer> <file>", args[0]);
        eprintln!("  <lexer>: regex | manual");
        return;
    }

    let lexer = &args[1];
    let file = &args[2];

    let input = std::fs::read_to_string(file).expect("Failed to read input file");

    let tokens: Vec<Token> = match lexer.as_str() {
        "regex" => match lex_with_regex(&input) {
            Ok(toks) => toks,
            Err(e) => {
                eprintln!(
                    "Regex Lexer Error at line {}, column {}: {}",
                    e.line, e.column, e.message
                );
                std::process::exit(1);
            }
        },
        "manual" => match lex_manually(&input) {
            Ok(toks) => toks,
            Err(e) => {
                eprintln!(
                    "Manual Lexer Error at line {}, column {}: {}",
                    e.line, e.column, e.message
                );
                std::process::exit(1);
            }
        },
        _ => {
            eprintln!("Unknown lexer: {}. Use 'regex' or 'manual'.", lexer);
            return;
        }
    };

    for token in tokens {
        // Use a match statement to print the token correctly
        match token {
            // For StringLit, use the standard formatter to print the unescaped string
            Token::StringLit(s) => println!("StringLit(\"{}\")", s),
            // For all other tokens, use the Debug formatter
            _ => println!("{:?}", token),
        }
    }
}