// src/parser/bison_bridge.rs

use crate::lexer::token::Token;
use std::sync::Mutex;
use std::os::raw::c_char; 
unsafe extern "C" {
    pub fn yyparse() -> i32;
}

struct ParserState {
    tokens: Vec<Token>,
    current_index: usize,
}

static PARSER_STATE: Mutex<Option<ParserState>> = Mutex::new(None);

const TOKEN_IDS: [(&str, i32); 51] = [ 
    // Keywords
    ("FUNCTION", 258), ("INT", 259), ("FLOAT", 260), ("STRING", 261), 
    ("BOOL", 262), ("RETURN", 263), ("IF", 264), ("ELSE", 265), 
    ("WHILE", 266), ("FOR", 267), ("TRUE", 268), ("FALSE", 269),
    // Identifiers and Literals
    ("IDENTIFIER", 270), ("INTLIT", 271), ("FLOATLIT", 272), ("STRINGLIT", 273),
    // Operators
    ("ASSIGN", 274), ("EQUALS", 275), ("NOTEQUALS", 276), ("LESS", 277), 
    ("GREATER", 278), ("LESSEQ", 279), ("GREATEREQ", 280), ("PLUS", 281), 
    ("MINUS", 282), ("MUL", 283), ("DIV", 284), ("MOD", 285), 
    ("AND", 286), ("OR", 287), ("NOT", 288), 
    // Bitwise Operators
    ("BITAND", 289), ("BITOR", 290), ("BITXOR", 291), ("BITNOT", 292), 
    ("SHIFTLEFT", 293), ("SHIFTRIGHT", 294), 
    // Increment/Decrement/Compound Assignment
    ("INCREMENT", 295), ("DECREMENT", 296), ("PLUSASSIGN", 297), ("MINUSASSIGN", 298), 
    // Delimiters
    ("PARENL", 299), ("PARENR", 300), ("BRACEL", 301), ("BRACER", 302), 
    ("BRACKETL", 303), ("BRACKETR", 304), ("COMMA", 305), ("SEMICOLON", 306), 
    ("COLON", 307),
    // EOF (This accounts for the 51st entry, which is correct for the C token list)
    ("EOF_TOKEN", 308),
];

fn token_to_id(token: &Token) -> i32 {
    let name = match token {
        Token::Function => "FUNCTION", Token::Int => "INT", Token::Float => "FLOAT",
        Token::String => "STRING", Token::Bool => "BOOL", Token::Return => "RETURN",
        Token::If => "IF", Token::Else => "ELSE", Token::While => "WHILE",
        Token::For => "FOR", Token::True => "TRUE", Token::False => "FALSE",
        Token::Identifier(_) => "IDENTIFIER", Token::IntLit(_) => "INTLIT",
        Token::FloatLit(_) => "FLOATLIT", Token::StringLit(_) => "STRINGLIT",
        Token::Assign => "ASSIGN", Token::Equals => "EQUALS", Token::NotEquals => "NOTEQUALS",
        Token::Less => "LESS", Token::Greater => "GREATER", Token::LessEq => "LESSEQ",
        Token::GreaterEq => "GREATEREQ", Token::Plus => "PLUS", Token::Minus => "MINUS",
        Token::Mul => "MUL", Token::Div => "DIV", Token::Mod => "MOD",
        Token::And => "AND", Token::Or => "OR", Token::Not => "NOT",
        Token::BitAnd => "BITAND", Token::BitOr => "BITOR", Token::BitXor => "BITXOR",
        Token::BitNot => "BITNOT", Token::ShiftLeft => "SHIFTLEFT", Token::ShiftRight => "SHIFTRIGHT",
        Token::Increment => "INCREMENT", Token::Decrement => "DECREMENT",
        Token::PlusAssign => "PLUSASSIGN", Token::MinusAssign => "MINUSASSIGN",
        Token::ParenL => "PARENL", Token::ParenR => "PARENR", Token::BraceL => "BRACEL",
        Token::BraceR => "BRACER", Token::BracketL => "BRACKETL", Token::BracketR => "BRACKETR",
        Token::Comma => "COMMA", Token::Semicolon => "SEMICOLON", Token::Colon => "COLON",
        Token::EOF => "EOF_TOKEN",
        Token::Invalid(s) => panic!("Invalid token found: {}", s),
    };
    
    TOKEN_IDS.iter().find(|(n, _)| *n == name)
        .map(|(_, id)| *id)
        .unwrap_or_else(|| panic!("Token ID not found for: {}", name))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rust_yylex(yylval_ptr: *mut u64) -> i32 {
    let mut state_guard = PARSER_STATE.lock().unwrap();
    let state = state_guard.as_mut().expect("Parser state not initialized.");

    if state.current_index >= state.tokens.len() {
        return 0; // Return 0 for Bison EOF
    }

    let token = state.tokens[state.current_index].clone();
    state.current_index += 1;

    let token_id = token_to_id(&token);
    
    match token {
        Token::Identifier(s) | Token::StringLit(s) => {
            let c_string = std::ffi::CString::new(s).unwrap();
            let c_ptr = c_string.into_raw();
            std::ptr::write_volatile(yylval_ptr.offset(0) as *mut *mut c_char, c_ptr);
        }
        Token::IntLit(i) => {
            // Field 1: long long int_val
            std::ptr::write_volatile(yylval_ptr.offset(1) as *mut i64, i);
        }
        Token::FloatLit(f) => {
            // Field 2: double float_val
            std::ptr::write_volatile(yylval_ptr.offset(2) as *mut f64, f);
        }
        _ => {
        }
    }

    token_id
}


pub fn parse_with_bison(tokens: Vec<Token>) -> Result<(), String> {
    // Initialize the global state
    {
        let mut state_guard = PARSER_STATE.lock().unwrap();
        *state_guard = Some(ParserState {
            tokens,
            current_index: 0,
        });
    }

    // Call the C parser entry point within an unsafe block
    let result_code = unsafe {
        yyparse()
    };

    // Clear the global state
    {
        let mut state_guard = PARSER_STATE.lock().unwrap();
        *state_guard = None;
    }

    // Check the result
    match result_code {
        0 => Ok(()), // yyparse returns 0 on success
        1 => Err(String::from("Bison Parse Failed (Syntax Error)")),
        _ => Err(format!("Bison Parse Failed with unexpected code: {}", result_code)),
    }
}