use std::fmt::Display;

// Todo: Wouldn't it be neat to not use object wrapper
#[derive(Debug, Eq, PartialEq)]
pub enum Object {
    Int(Integer),
    Bool(Boolean),
    Null,
}

impl Object {
    pub fn int(value: i64) -> Object {
        Object::Int(Integer { value })
    }

    pub fn bool(value: bool) -> Object {
        if value {
            Object::Bool(Boolean::True)
        } else {
            Object::Bool(Boolean::False)
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
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
