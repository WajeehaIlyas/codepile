mod lexer;
mod parser;
mod utils;

use lexer::manual_lex::lex_manually;
use lexer::regex_lex::lex_with_regex;
use lexer::token::Token;
use parser::parser::Parser;
use parser::ast;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Check for correct command-line arguments.
    if args.len() < 3 {
        eprintln!("Usage: {} <lexer> <file>", args[0]);
        eprintln!("  <lexer>: regex | manual");
        std::process::exit(1);
    }

    let lexer_choice = &args[1];
    let file_path = &args[2];

    // Read the source code file.
    let input = std::fs::read_to_string(file_path)
        .expect("Failed to read input file");

    // Run the specified lexer on the input to get a token stream.
    let tokens: Vec<Token> = match lexer_choice.as_str() {
        "regex" => match lex_with_regex(&input) {
            Ok(toks) => toks,
            Err(e) => {
                eprintln!(
                    "Regex Lexer Error: at line {}, column {}: {}",
                    e.line, e.column, e.message
                );
                std::process::exit(1);
            }
        },
        "manual" => match lex_manually(&input) {
            Ok(toks) => toks,
            Err(e) => {
                eprintln!(
                    "Manual Lexer Error: at line {}, column {}: {}",
                    e.line, e.column, e.message
                );
                std::process::exit(1);
            }
        },
        _ => {
            eprintln!("Unknown lexer: {}. Use 'regex' or 'manual'.", lexer_choice);
            std::process::exit(1);
        }
    };

    println!("Lexing successful! Tokens generated:\n{:?}\n", tokens);

    // Pass the resulting tokens to the parser.
    let mut parser = Parser::new(tokens);

    match parser.parse_program() {
        Ok(program_ast) => {
            println!("Parsing successful! Here is the Abstract Syntax Tree:\n");
            let formatted_ast = ast::format_program(&program_ast);
            println!("{}", formatted_ast);
        }
        Err(e) => {
            eprintln!("Parse error: {}", e);
        }
    }
}