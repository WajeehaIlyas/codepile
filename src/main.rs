// src/main.rs

#![allow(warnings)]

mod lexer;
mod parser;
mod utils;

use lexer::manual_lex::lex_manually;
use lexer::regex_lex::lex_with_regex;
use lexer::token::Token;
use parser::parser::Parser;
use parser::ast;
use parser::bison_bridge;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Check for correct command-line arguments (lexer, parser, file).
    if args.len() < 4 {
        eprintln!("Usage: {} <lexer> <parser> <file>", args[0]);
        eprintln!("  <lexer>: regex | manual");
        eprintln!("  <parser>: recursive | bison");
        std::process::exit(1);
    }

    let lexer_choice = &args[1];
    let parser_choice = &args[2];
    let file_path = &args[3];

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
        "manual" => match lex_manually(&input) { // <-- FIX: Handles the Result<Vec<Token>, LexError>
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

    // Run the specified parser.
    match parser_choice.as_str() {
        "recursive" => {
            // Pass the resulting tokens to the recursive descent parser.
            let mut parser = Parser::new(tokens);
            match parser.parse_program() {
                Ok(program_ast) => {
                    println!("Recursive Descent Parsing successful! Here is the Abstract Syntax Tree:\n");
                    let formatted_ast = ast::format_program(&program_ast);
                    println!("{}", formatted_ast);
                }
                Err(e) => {
                    eprintln!("Recursive Descent Parse error: {}", e);
                }
            }
        }
        "bison" => {
            // Call the Bison parser bridge
            match bison_bridge::parse_with_bison(tokens) {
                Ok(_) => {
                    println!("Bison Parsing successful! (Syntax validated)");
                }
                Err(e) => {
                    eprintln!("Bison Parse error: {}", e);
                }
            }
        }
        _ => {
            eprintln!("Unknown parser: {}. Use 'recursive' or 'bison'.", parser_choice);
            std::process::exit(1);
        }
    }
}