#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    // Identifiers + literals
    Ident(String),
    Int(String),
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    // delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn lookup_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        ident => Token::Ident(ident.to_string()),
    }
}
