use std::{
    fmt::{Display, Write},
    rc::Rc,
};

use anyhow::anyhow;

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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(Expression),
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(inner) => write!(f, "{}", inner),
            Statement::ReturnStatement(inner) => write!(f, "{}", inner),
            Statement::ExpressionStatement(inner) => write!(f, "{}", inner),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Callee {
    FunctionLiteral(FunctionLiteral),
    Identifier(Identifier),
}
impl Display for Callee {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callee::FunctionLiteral(function_literal) => {
                write!(f, "{}", function_literal)
            }
            Callee::Identifier(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Callee,
    pub arguments: Vec<Expression>,
}
impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            &self.function,
            &self
                .arguments
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Identifier(Identifier),
    Boolean(Boolean),
    Integer(IntegerLiteral),
    FunctionLiteral(FunctionLiteral),
    Call(CallExpression),
}
impl TryInto<Callee> for Expression {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Callee, Self::Error> {
        match self {
            Expression::Identifier(identifier) => Ok(Callee::Identifier(identifier)),
            Expression::FunctionLiteral(function_literal) => {
                Ok(Callee::FunctionLiteral(function_literal))
            }
            expr => Err(anyhow!("Could not convert {} into a function call", expr)),
        }
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(literal) => write!(f, "{}", literal.to_string()),
            Expression::Boolean(literal) => write!(f, "{}", literal.to_string()),
            Expression::Integer(literal) => write!(f, "{}", literal.to_string()),
            Expression::PrefixExpression(expr) => write!(f, "{}", expr.to_string()),
            Expression::InfixExpression(expr) => write!(f, "{}", expr.to_string()),
            Expression::FunctionLiteral(expr) => write!(f, "{}", expr),
            Expression::Call(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}
impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer: String = String::with_capacity(1024);
        for s in self.statements.iter() {
            buffer.write_str(&s.to_string())?;
        }
        write!(f, "{{ {} }}", buffer)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Rc<Expression>,
}
impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Rc<Expression>,
    pub operator: String,
    pub right: Rc<Expression>,
}
impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{}{})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}
impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}
impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub paramaters: Vec<Identifier>,
    pub body: BlockStatement,
}
impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.paramaters
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>()
                .join(","),
            self.body
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
