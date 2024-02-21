use std::fmt::{Debug, Display};

use crate::ast;
use crate::eval::Environment;

// Todo: Wouldn't it be neat to not use object wrapper
#[derive(Eq, PartialEq, Clone)]
pub enum Object {
    Int(Integer),
    Bool(Boolean),
    Fn(Function),
    Return(Box<Object>),
    Null,
}

impl Object {
    pub fn int(value: i64) -> Object {
        Object::Int(Integer { value })
    }

    pub fn func_with_env(
        parameters: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    ) -> Object {
        Object::Fn(Function {
            parameters,
            body,
            env,
        })
    }

    pub fn func(parameters: Vec<ast::Identifier>, body: ast::BlockStatement) -> Object {
        let env = Environment::new();
        Object::Fn(Function {
            parameters,
            body,
            env,
        })
    }

    pub fn func_str(args: Vec<&str>, body: ast::BlockStatement) -> Object {
        let env = Environment::new();
        let args_identifiers: Vec<_> = args
            .iter()
            .map(|name| ast::Identifier::from_str(name))
            .collect();
        Object::Fn(Function {
            parameters: args_identifiers,
            body,
            env,
        })
    }

    pub fn bool(value: bool) -> Object {
        if value {
            Object::Bool(Boolean::True)
        } else {
            Object::Bool(Boolean::False)
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}::<INTEGER>", value),
            Object::Bool(bool) => write!(f, "{}::<BOOLEAN>", bool),
            Object::Null => write!(f, "NULL"),
            Object::Fn(func) => write!(f, "{func}::<FUNCTION>"),
            Object::Return(_) => unreachable!(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
            Object::Fn(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}
impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Integer { value } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Environment,
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {{ {} }}",
            self.parameters
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>()
                .join(","),
            self.body
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Boolean {
    True,
    False,
}

impl Into<bool> for Boolean {
    fn into(self) -> bool {
        match self {
            Boolean::True => true,
            Boolean::False => false,
        }
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
        }
    }
}
