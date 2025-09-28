// mod lexer;
// mod utils;
// mod parser;

// use lexer::manual_lex::lex_manually;
// use lexer::regex_lex::lex_with_regex;
// use lexer::token::Token;

// fn main() {
//     let args: Vec<String> = std::env::args().collect();

//     if args.len() < 3 {
//         eprintln!("Usage: {} <lexer> <file>", args[0]);
//         eprintln!("  <lexer>: regex | manual");
//         return;
//     }

//     let lexer = &args[1];
//     let file = &args[2];

//     let input = std::fs::read_to_string(file).expect("Failed to read input file");

//     let tokens: Vec<Token> = match lexer.as_str() {
//         "regex" => match lex_with_regex(&input) {
//             Ok(toks) => toks,
//             Err(e) => {
//                 eprintln!(
//                     "Regex Lexer Error at line {}, column {}: {}",
//                     e.line, e.column, e.message
//                 );
//                 std::process::exit(1);
//             }
//         },
//         "manual" => match lex_manually(&input) {
//             Ok(toks) => toks,
//             Err(e) => {
//                 eprintln!(
//                     "Manual Lexer Error at line {}, column {}: {}",
//                     e.line, e.column, e.message
//                 );
//                 std::process::exit(1);
//             }
//         },
//         _ => {
//             eprintln!("Unknown lexer: {}. Use 'regex' or 'manual'.", lexer);
//             return;
//         }
//     };

//     for token in tokens {
//         // Use a match statement to print the token correctly
//         match token {
//             // For StringLit, use the standard formatter to print the unescaped string
//             Token::StringLit(s) => println!("StringLit(\"{}\")", s),
//             // For all other tokens, use the Debug formatter
//             _ => println!("{:?}", token),
//         }
//     }
// }



// src/main.rs
// Small test harness that constructs token vectors by hand and runs the parser.
//
// Adjust module paths only if your project layout is different.

mod lexer;
mod parser;
mod utils;

use crate::lexer::token::Token;
use crate::parser::parser::Parser;
use crate::parser::ast;

fn run_test(name: &str, tokens: Vec<Token>) {
    println!("=== TEST: {} ===", name);
    let mut parser = Parser::new(tokens);
    match parser.parse_program() {
        Ok(prog) => {
            let out = ast::format_program(&prog);
            println!("{}", out);
        }
        Err(e) => {
            eprintln!("Parse error: {}", e);
        }
    }
    println!();
}



//WE WRAP INSIDE int main() BECAUSE OUR GRAMMAR EXPECTS A DECLARATAION FIRST, EITHER VARIABLE OR FUNCTION.
//Hence we need to wrap it inside a function delcaration for it to be valid, as per our grammar. 
fn main() {
    // 1) Variable declaration: int x = 5;
    let tokens_var = vec![
        Token::Int,
        Token::Identifier("x".into()),
        Token::Assign,
        Token::IntLit(5),
        Token::Semicolon,
        Token::EOF,
    ];
    run_test("var_decl: int x = 5;", tokens_var);

    // 2) Function: int add(int a, int b) { return a + b; }
    let tokens_fn = vec![
        Token::Int,
        Token::Identifier("add".into()),
        Token::ParenL,
            Token::Int, Token::Identifier("a".into()), Token::Comma,
            Token::Int, Token::Identifier("b".into()),
        Token::ParenR,
        Token::BraceL,
            Token::Return,
            Token::Identifier("a".into()),
            Token::Plus,
            Token::Identifier("b".into()),
            Token::Semicolon,
        Token::BraceR,
        Token::EOF,
    ];
    run_test("function: int add(int a, int b) { return a + b; }", tokens_fn);

    // 3) For loop wrapped inside a function so it's a valid top-level declaration:
    //    int main() { for (i = 0; i < 10; i = i + 1) { x = x + 1; } }
    let tokens_for = vec![
        Token::Int,
        Token::Identifier("main".into()),
        Token::ParenL,
        Token::ParenR,
        Token::BraceL,
            Token::For,
            Token::ParenL,
                Token::Identifier("i".into()),
                Token::Assign,
                Token::IntLit(0),
                Token::Semicolon,
                Token::Identifier("i".into()),
                Token::Less,
                Token::IntLit(10),
                Token::Semicolon,
                Token::Identifier("i".into()),
                Token::Assign,
                Token::Identifier("i".into()),
                Token::Plus,
                Token::IntLit(1),
            Token::ParenR,
            Token::BraceL,
                Token::Identifier("x".into()),
                Token::Assign,
                Token::Identifier("x".into()),
                Token::Plus,
                Token::IntLit(1),
                Token::Semicolon,
            Token::BraceR,
        Token::BraceR,
        Token::EOF,
    ];
    run_test("for loop (wrapped in int main)", tokens_for);

    // 4) Assignment chaining wrapped inside a function:
    //    int main() { a = b = 3; }
    let tokens_chain = vec![
        Token::Int,
        Token::Identifier("main".into()),
        Token::ParenL,
        Token::ParenR,
        Token::BraceL,
            Token::Identifier("a".into()),
            Token::Assign,
            Token::Identifier("b".into()),
            Token::Assign,
            Token::IntLit(3),
            Token::Semicolon,
        Token::BraceR,
        Token::EOF,
    ];
    run_test("assignment chaining (wrapped in int main)", tokens_chain);
}

