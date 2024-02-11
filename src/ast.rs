use std::fmt::{Display, Write};

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{}", buffer)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(inner) => write!(f, "{}", inner.to_string()),
        }
    }
}
#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Boolean(Boolean),
    Integer(IntegerLiteral),
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(inner) => write!(f, "{}", inner.to_string()),
            Expression::Boolean(inner) => write!(f, "{}", inner.to_string()),
            Expression::Integer(inner) => write!(f, "{}", inner.to_string()),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Identifier {
    pub value: String,
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Boolean {
    pub value: bool,
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statement_can_be_displayed() {
        let statement = LetStatement {
            name: Identifier {
                value: "myVar".to_string(),
            },
            value: Expression::Identifier(Identifier {
                value: "anotherVar".to_string(),
            }),
        };
        assert_eq!(statement.to_string(), "let myVar = anotherVar;")
    }
}
