use std::fmt::Display;

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

use Token::*;
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Illegal => write!(f, "ILLEGAL"),
            EOF => write!(f, "EOF"),
            // Identifiers + literals
            Ident(_) => write!(f, "IDENT"),
            Int(_) => write!(f, "INT"),
            // Operators
            Assign => write!(f, "="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Bang => write!(f, "!"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            // delimiters
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            // keywords
            Function => write!(f, "FUNCTION"),
            Let => write!(f, "LET"),
            True => write!(f, "TRUE"),
            False => write!(f, "FALSE"),
            If => write!(f, "IF"),
            Else => write!(f, "ELSE"),
            Return => write!(f, "RETURN"),
        }
    }
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
