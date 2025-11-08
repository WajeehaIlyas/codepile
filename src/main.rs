// src/main.rs

#![allow(warnings)]

mod lexer;
mod parser;
mod utils;
mod scope;

use lexer::manual_lex::lex_manually;
use lexer::regex_lex::lex_with_regex;
use lexer::token::Token;
use parser::parser::Parser;
use parser::ast;
use parser::bison_bridge;
use scope::analyzer::ScopeAnalyzer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 4 {
        eprintln!("Usage: {} <lexer> <parser> <file>", args[0]);
        eprintln!("  <lexer>: regex | manual");
        eprintln!("  <parser>: recursive | bison");
        std::process::exit(1);
    }

    let lexer_choice = &args[1];
    let parser_choice = &args[2];
    let file_path = &args[3];

    let input = std::fs::read_to_string(file_path)
        .expect("Failed to read input file");

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

    match parser_choice.as_str() {
        "recursive" => {
            let mut parser = Parser::new(tokens);
            match parser.parse_program() {
                Ok(program_ast) => {
                    println!("Recursive Descent Parsing successful! Here is the Abstract Syntax Tree:\n");
                    let formatted_ast = ast::format_program(&program_ast);
                    println!("{}", formatted_ast);

                    let mut analyzer = ScopeAnalyzer::new();
                    match analyzer.analyze_program(&program_ast) {
                        Ok(_) => {
                            println!("Scope Analysis successful!");
                        }
                        Err(errors) => {
                            eprintln!("Scope Analysis FAILED with {} errors:", errors.len());
                            for e in errors {
                                eprintln!("{}", e);
                            }
                            std::process::exit(1); 
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Recursive Descent Parse error: {}", e);
                }
            }
        }
        "bison" => {
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