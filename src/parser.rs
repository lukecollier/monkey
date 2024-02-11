use crate::{
    ast::{Boolean, Expression, Identifier, IntegerLiteral, LetStatement, Program, Statement},
    lexer::Lexer,
    token::Token,
};
use anyhow::*;

#[derive(PartialEq, PartialOrd, Ord, Eq)]
enum Precedence {
    Lowest,
    Equals,
    LessGraeter,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedence(token: &Token) -> Precedence {
    use Precedence::*;
    match token {
        Token::Eq => Equals,
        Token::NotEq => Equals,
        Token::Lt => LessGraeter,
        Token::Gt => LessGraeter,
        Token::Plus => Sum,
        Token::Minus => Sum,
        Token::Slash => Product,
        Token::Asterisk => Product,
        Token::LParen => Call,
        _ => Lowest,
    }
}
#[derive(Debug)]
struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(l: Lexer<'a>) -> Parser<'a> {
        let mut p = Self {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            // Todo: Estimate capacity required better
            statements: Vec::with_capacity(512),
        };
        while self.cur_token != Token::EOF {
            if let Result::Ok(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            };
            self.next_token()
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => {
                let s = self.parse_let_statement()?;
                Ok(Statement::LetStatement(s))
            }
            _ => todo!("hooow"),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        let name = match &self.peek_token {
            Token::Ident(ident) => Identifier {
                value: ident.to_string(),
            },
            token => {
                return Err(anyhow!(
                    "expected next token to be {} but got {} instead",
                    Token::Ident("".to_string()),
                    token
                ))
            }
        };
        self.next_token();
        match &self.peek_token {
            Token::Assign => (),
            token => {
                return Err(anyhow!(
                    "expected next token to be {} but got {} instead",
                    Token::Assign,
                    token
                ))
            }
        };
        self.next_token();
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if let Token::Semicolon = &self.peek_token {
        } else {
            return Err(anyhow!(
                "expected next token to be {} but got {} instead",
                Token::Assign,
                &self.peek_token
            ));
        };
        self.next_token();
        Ok(LetStatement { value, name })
    }

    fn parse_prefix(token: &Token) -> Result<Expression> {
        Ok(match token {
            Token::Ident(name) => Expression::Identifier(Identifier {
                value: name.to_string(),
            }),
            Token::Int(int_str) => {
                let value = int_str.parse::<i64>()?;
                Expression::Integer(IntegerLiteral { value })
            }
            Token::True => Expression::Boolean(Boolean { value: true }),
            Token::False => Expression::Boolean(Boolean { value: false }),
            _ => todo!("need an error"),
        })
    }

    fn parse_infix(token: &Token) -> Result<Expression> {
        match token {
            Token::Ident(name) => Ok(Expression::Identifier(Identifier {
                value: name.to_string(),
            })),
            Token::Int(int_str) => {
                let value = int_str.parse::<i64>()?;
                Ok(Expression::Integer(IntegerLiteral { value }))
            }
            Token::True => Ok(Expression::Boolean(Boolean { value: true })),
            Token::False => Ok(Expression::Boolean(Boolean { value: false })),
            _ => todo!("need an error"),
        }
    }

    fn parse_expression(&mut self, precendence: Precedence) -> Result<Expression> {
        let left_expr = Self::parse_prefix(&self.cur_token)?;
        while !self.peek_token_is(&Token::Semicolon) && &precendence < &self.peek_precedence() {
            // if let Some(infix) = Self::parse_infix(&self.peek_token) {}
        }
        Ok(left_expr)
    }

    fn peek_precedence(&self) -> Precedence {
        precedence(&self.peek_token)
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        return std::mem::discriminant(&self.peek_token) == std::mem::discriminant(token);
    }

    fn next_token(&mut self) {
        // todo: Fix this clone
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }
}

#[cfg(test)]
mod tests {

    use crate::ast::{
        Boolean, Expression, Identifier, IntegerLiteral, LetStatement, Program, Statement,
    };

    use super::*;

    fn let_statement<'a>(identifier: &'a str, value: Expression) -> Program {
        Program {
            statements: vec![Statement::LetStatement(LetStatement {
                name: Identifier {
                    value: identifier.to_string(),
                },
                value,
            })],
        }
    }

    #[test]
    fn let_statements_parse_correctly() {
        let parsed: [Program; 3] =
            ["let x = 5;", "let y = true;", "let foobar = y;"].map(|statement| {
                let lexer = crate::lexer::Lexer::new(statement);
                let mut parser = Parser::new(lexer);
                parser.parse_program()
            });
        let expected = [
            let_statement("x", Expression::Integer(IntegerLiteral { value: 5 })),
            let_statement("y", Expression::Boolean(Boolean { value: true })),
            let_statement(
                "foobar",
                Expression::Identifier(Identifier {
                    value: "y".to_string(),
                }),
            ),
        ];
        assert_eq!(parsed, expected);
    }
}
