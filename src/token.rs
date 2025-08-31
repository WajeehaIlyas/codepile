#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Function,
    Int,
    Float,
    String,
    Bool,
    Return,
    If,
    Else,
    While,
    For,
    True,
    False,

    // Identifiers
    Identifier(String),

    // Literals
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),

    // Operators
    Assign,    // =
    Equals,    // ==
    NotEquals, // !=
    Less,      // <
    Greater,   // >
    LessEq,    // <=
    GreaterEq, // >=
    Plus,      // +
    Minus,     // -
    Mul,       // *
    Div,       // /
    Mod,       // %
    And,       // &&
    Or,        // ||
    Not,       // !

    //Bitwise Operators
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    BitNot,     // ~
    ShiftLeft,  // <<
    ShiftRight, // >>
    // Increment/Decrement
    Increment, // ++
    Decrement, // --
    // Compound Assignment
    PlusAssign,  // +=
    MinusAssign, // -=

    // Delimiters & Symbols
    ParenL,    // (
    ParenR,    // )
    BraceL,    // {
    BraceR,    // }
    BracketL,  // [
    BracketR,  // ]
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :

    EOF,

    Invalid(String),
}
