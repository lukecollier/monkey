use crate::token::{lookup_ident, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::EOF => None,
            token => Some(token),
        }
    }
}

// todo: Impl iterator for this
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn peek_char(&mut self) -> Option<u8> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char()
        }
        return std::str::from_utf8(&self.input[position..self.position]).unwrap();
        // todo! Error handling
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char()
        }
        return std::str::from_utf8(&self.input[position..self.position]).unwrap();
        // todo! Error handling
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        if self.read_position > self.input.len() {
            return Token::EOF;
        }
        let token = match self.ch {
            b'+' => Token::Plus,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'-' => Token::Minus,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'>' => Token::Gt,
            b'<' => Token::Lt,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'=' => {
                if self.peek_char().is_some_and(|byte| byte == b'=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'!' => {
                if self.peek_char().is_some_and(|byte| byte == b'=') {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            _ => {
                if self.ch.is_ascii_alphabetic() {
                    return lookup_ident(self.read_identifier());
                } else if self.ch.is_ascii_digit() {
                    return Token::Int(self.read_number().to_string());
                } else {
                    Token::Illegal
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::*;

    #[test]
    fn next_number() {
        let source = r#"let b = 500;"#;
        let expected_tokens: [Token; 5] = [
            Token::Let,
            Token::Ident("b".to_string()),
            Token::Assign,
            Token::Int(500.to_string()),
            Token::Semicolon,
        ];
        let lexer = Lexer::new(source);
        let tokens = lexer.into_iter().collect::<Vec<_>>();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn ignores_whitespace_as_god_intended() {
        let source = r#"      =       +     "#;
        let expected_tokens: [Token; 2] = [Token::Assign, Token::Plus];
        let lexer = Lexer::new(source);
        let tokens = lexer.into_iter().collect::<Vec<_>>();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn can_parse_sumbols() {
        let source = r#"(){}-+=,;><*/!"#;

        let expected_tokens: [Token; 14] = [
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Minus,
            Token::Plus,
            Token::Assign,
            Token::Comma,
            Token::Semicolon,
            Token::Gt,
            Token::Lt,
            Token::Asterisk,
            Token::Slash,
            Token::Bang,
        ];

        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Vec<_>>();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn can_parse_a_source_program() {
        let source = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
        return true;
        } else {
        return false;
        }

        10 == 10; 
        10 != 9;"#;

        let expected_tokens: [Token; 73] = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10.to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::Gt,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10.to_string()),
            Token::Eq,
            Token::Int(10.to_string()),
            Token::Semicolon,
            Token::Int(10.to_string()),
            Token::NotEq,
            Token::Int(9.to_string()),
            Token::Semicolon,
        ];

        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Vec<_>>();
        assert_eq!(tokens, expected_tokens);
    }
}
