use std::collections::HashMap;

use crate::ast::*;
use crate::object::{Boolean, Integer, Object};
use anyhow::*;

// I think scope is a cooler name
// More efficient then this we can hold a single HashMap that and hash a scope id as well
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}
impl Environment {
    // here we push to the top scope unless we
    pub fn set(&mut self, key: String, value: Object) -> Object {
        self.store.insert(key, value.clone());
        value
    }

    pub fn get(&mut self, key: &String) -> Option<Object> {
        //reverse search so we find the first matching instance
        if let Some(outer) = self.outer.as_mut() {
            (*outer)
                .get(key)
                .map(|obj| obj.to_owned())
                .or(self.store.get(key).map(|obj| obj.to_owned()))
        } else {
            self.store.get(key).map(|obj| obj.to_owned())
        }
    }

    pub fn enclose(self) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(self.clone().into());
        env
    }

    pub fn new() -> Environment {
        Environment {
            // the default scope
            store: HashMap::with_capacity(128),
            outer: None,
        }
    }
}
pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Result<Object>;
}

// :think: we can use a result and treat the error path as a return early
// it seems pretty meh though, we can also include an interrupt flag?
//
// resulting Error type would be an Enum of Interrupt and Failure
// type EvalError = Result<Object, Interrupt>;
// enum Interrupt {
//     Return(Object),
//     Error(anyhow::Error),
// }

impl Eval for PrefixExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match (&self.operator, self.right.eval(env)?) {
            (crate::ast::PrefixOperator::Minus, Object::Int(Integer { value })) => {
                // todo: implicit clone :(
                Ok(Object::Int(Integer { value: -value }))
            }
            (crate::ast::PrefixOperator::Bang, Object::Bool(Boolean::True)) => {
                Ok(Object::Bool(Boolean::False))
            }
            (crate::ast::PrefixOperator::Bang, Object::Bool(Boolean::False)) => {
                Ok(Object::Bool(Boolean::True))
            }
            (crate::ast::PrefixOperator::Bang, Object::Null) => Ok(Object::Bool(Boolean::True)),
            (crate::ast::PrefixOperator::Bang, _) => Ok(Object::Bool(Boolean::False)),
            // todo: improve errors! no null
            (crate::ast::PrefixOperator::Minus, bool @ Object::Bool(_)) => {
                Err(anyhow!("unknown operator: -{:?}", bool))
            }
            (crate::ast::PrefixOperator::Minus, Object::Null) => {
                Err(anyhow!("unknown operator: -NULL"))
            }
            (crate::ast::PrefixOperator::Minus, func @ Object::Fn(_)) => {
                Err(anyhow!("unknown operator: -{:?}", func))
            }
            (crate::ast::PrefixOperator::Minus, ret @ Object::Return(_)) => Ok(ret),
        }
    }
}

fn eval_infix_integers(left: Integer, op: &InfixOperator, right: Integer) -> Object {
    match op {
        // Maths
        InfixOperator::Plus => Object::int(left.value + right.value),
        InfixOperator::Minus => Object::int(left.value - right.value),
        InfixOperator::Multiply => Object::int(left.value * right.value),
        InfixOperator::Divide => Object::int(left.value / right.value),
        // Boolean
        InfixOperator::Gt => Object::bool(left.value > right.value),
        InfixOperator::Lt => Object::bool(left.value < right.value),
        InfixOperator::Eq => Object::bool(left.value == right.value),
        InfixOperator::NotEq => Object::bool(left.value != right.value),
    }
}
fn eval_infix_booleans(left: Boolean, op: &InfixOperator, right: Boolean) -> Result<Object> {
    match op {
        InfixOperator::Gt => {
            let left_bool: bool = left.into();
            let right_bool: bool = right.into();
            Ok(Object::bool(left_bool > right_bool))
        }
        InfixOperator::Lt => {
            let left_bool: bool = left.into();
            let right_bool: bool = right.into();
            Ok(Object::bool(left_bool < right_bool))
        }
        InfixOperator::Eq => {
            let left_bool: bool = left.into();
            let right_bool: bool = right.into();
            Ok(Object::bool(left_bool == right_bool))
        }
        InfixOperator::NotEq => {
            let left_bool: bool = left.into();
            let right_bool: bool = right.into();
            Ok(Object::bool(left_bool != right_bool))
        }
        op => Err(anyhow!(
            "unknown operator: {}::<BOOLEAN> {} {}::<BOOLEAN>",
            left,
            op,
            right
        )),
    }
}
impl Eval for InfixExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match (self.left.eval(env)?, &self.operator, self.right.eval(env)?) {
            (Object::Int(left), op, Object::Int(right)) => Ok(eval_infix_integers(left, op, right)),
            (Object::Bool(left), op, Object::Bool(right)) => eval_infix_booleans(left, op, right),
            (left @ Object::Int(_), op, right) => {
                Err(anyhow!("type mismatch: {:?} {} {:?}", left, op, right))
            }
            (left, op, right) => Err(anyhow!("unknown operator: {:?} {} {:?}", left, op, right)),
        }
    }
}

impl Eval for BlockStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in &self.statements {
            match stmt.eval(env)? {
                // TODO: this is iind of gross, maybe theres a cooler way to return
                // object could have a "interupt" flag we can set that's ready here
                obj @ Object::Return(_) => return Ok(obj),
                obj => result = obj,
            }
        }
        Ok(result)
    }
}

impl Eval for IfExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        let condition = self.condition.eval(env)?;
        match condition {
            Object::Int(_) => self.consequence.eval(env),
            Object::Bool(Boolean::True) => self.consequence.eval(env),
            Object::Bool(Boolean::False) => self.alternative.eval(env),
            Object::Null => self.alternative.eval(env),
            rest => Ok(rest),
        }
    }
}

// todo: Can we derive eval if the constituants implement eval? :think:
impl Eval for Expression {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match self {
            Expression::PrefixExpression(prefix_expr) => prefix_expr.eval(env),
            Expression::InfixExpression(infix_expr) => infix_expr.eval(env),
            Expression::Identifier(identifier) => identifier.eval(env),
            Expression::Boolean(crate::ast::Boolean { value: true }) => {
                Ok(Object::Bool(Boolean::True))
            }
            Expression::Boolean(crate::ast::Boolean { value: false }) => {
                Ok(Object::Bool(Boolean::False))
            }
            Expression::IntegerLiteral(literal) => Ok(Object::Int(Integer {
                value: literal.value,
            })),
            Expression::FunctionLiteral(function_literal) => function_literal.eval(env),
            Expression::CallExpression(call) => {
                let callee = call.function.eval(env)?;

                match callee {
                    Object::Fn(func) => {
                        let mut enclosed = func.env.enclose();
                        for (name, stmt) in func.parameters.iter().zip(&call.arguments) {
                            enclosed.set(name.value.to_string(), stmt.eval(env)?);
                        }
                        func.body.eval(&mut enclosed)
                    }
                    obj => Err(anyhow!("not a function: {:?}", obj)),
                }
            }
            Expression::IfExpression(if_expr) => if_expr.eval(env),
        }
    }
}

impl Eval for Identifier {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        env.get(&self.value)
            .ok_or(anyhow!("identifier not found: {}", self))
    }
}

impl Eval for FunctionLiteral {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        Ok(Object::func_with_env(
            self.paramaters.to_vec(),
            self.body.clone(),
            // this way of environments is pretty easy to break, we're just cloning the entire env
            // we need to infect all the calls with a lifetime so we can handle this case
            // it's different from the book but essentially works the same as this evaluator is
            // sync
            env.clone(),
        ))
    }
}

impl Eval for Callee {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match self {
            Callee::FunctionLiteral(function_literal) => function_literal.eval(env),
            Callee::Identifier(identifier) => identifier.eval(env),
        }
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        match self {
            Statement::LetStatement(let_stmt) => {
                let value = let_stmt.value.eval(env)?;
                Ok(env.set(let_stmt.name.to_string(), value))
            }
            Statement::ReturnStatement(return_stmt) => {
                Ok(Object::Return(return_stmt.return_value.eval(env)?.into()))
            }
            Statement::ExpressionStatement(expr_stmt) => expr_stmt.eval(env),
        }
    }
}

impl Eval for Program {
    fn eval(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Null;
        for stmt in &self.statements {
            match stmt.eval(env)? {
                Object::Return(content) => return Ok(*content),
                obj => result = obj,
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {

    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_errors() {
        let expectations = vec![
            (
                "true - true;",
                "unknown operator: true::<BOOLEAN> - true::<BOOLEAN>",
            ),
            (
                "true + true;",
                "unknown operator: true::<BOOLEAN> + true::<BOOLEAN>",
            ),
            (
                "true + false;",
                "unknown operator: true::<BOOLEAN> + false::<BOOLEAN>",
            ),
            ("5 + true;", "type mismatch: 5::<INTEGER> + true::<BOOLEAN>"),
            (
                "5 + true; 5;",
                "type mismatch: 5::<INTEGER> + true::<BOOLEAN>",
            ),
            ("-true", "unknown operator: -true::<BOOLEAN>"),
            (
                "true + false;",
                "unknown operator: true::<BOOLEAN> + false::<BOOLEAN>",
            ),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: true::<BOOLEAN> + false::<BOOLEAN>",
            ),
            (
                r#" if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
                return 1; }
            "#,
                "unknown operator: true::<BOOLEAN> + false::<BOOLEAN>",
            ),
            ("foobar", "identifier not found: foobar"),
        ];
        test_from_source_err(expectations);
    }

    #[test]
    fn test_function() -> Result<()> {
        let expectations = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::int(5)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::int(5),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Object::int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::int(10)),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::int(20),
            ),
            ("fn(x) { x; }(5)", Object::int(5)),
            (
                r#"
                let newAdder = fn(x) {
                    fn(y) { x + y };
                };
                let addTwo = newAdder(2);
                addTwo(2);
               "#,
                Object::int(4),
            ),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_let_function() -> Result<()> {
        use crate::ast::*;
        let expectations = vec![(
            "fn(x) { x + 2; };",
            Object::func_str(
                vec!["x"],
                BlockStatement::single(Statement::expression(Expression::infix_expression(
                    Expression::identifier("x"),
                    InfixOperator::Plus,
                    Expression::integer_literal(2),
                ))),
            ),
        )];
        test_from_source(expectations)
    }

    #[test]
    fn test_let_statement() -> Result<()> {
        let expectations = vec![
            ("let a = 5; a;", Object::int(5)),
            ("let a = 5 * 5; a;", Object::int(25)),
            ("let a = 5; let b = a; b;", Object::int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::int(15),
            ),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_return_statement() -> Result<()> {
        let expectations = vec![
            ("return 10;", Object::int(10)),
            ("return 10; 9;", Object::int(10)),
            ("return 2 * 5; 9;", Object::int(10)),
            ("9; return 2 * 5; 9;", Object::int(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::int(10),
            ),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_boolean_expression() -> Result<()> {
        let expectations = vec![
            ("false == true", Object::bool(false)),
            ("true == false", Object::bool(false)),
            ("true == true", Object::bool(true)),
            ("false == false", Object::bool(true)),
            ("false != true", Object::bool(true)),
            ("true != false", Object::bool(true)),
            ("true != true", Object::bool(false)),
            ("false != false", Object::bool(false)),
            ("5 < 5", Object::bool(false)),
            ("5 > 5", Object::bool(false)),
            ("5 < 6", Object::bool(true)),
            ("6 > 5", Object::bool(true)),
            ("1 == 1", Object::bool(true)),
            ("1 == 2", Object::bool(false)),
            ("1 != 1", Object::bool(false)),
            ("1 != 2", Object::bool(true)),
            ("(1 < 2) == true", Object::bool(true)),
            ("(1 < 2) == false", Object::bool(false)),
            ("(1 > 2) == true", Object::bool(false)),
            ("(1 > 2) == false", Object::bool(true)),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_if_expression() -> Result<()> {
        let expectations = vec![
            ("if (true) { 10 }", Object::int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::int(10)),
            ("if (1 < 2) { 10 }", Object::int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::int(10)),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_integer_expression() -> Result<()> {
        let expectations = vec![
            ("5", Object::int(5)),
            ("10", Object::int(10)),
            ("-5", Object::int(-5)),
            ("-10", Object::int(-10)),
            ("5 + 5 + 5 + 5 - 10", Object::int(10)),
            ("2 * 2 * 2 * 2 * 2", Object::int(32)),
            ("-50 + 100 + -50", Object::int(0)),
            ("5 * 2 + 10", Object::int(20)),
            ("5 + 2 * 10", Object::int(25)),
            ("20 + 2 * -10", Object::int(0)),
            ("50 / 2 * 2 + 10", Object::int(60)),
            ("2 * (5 + 10)", Object::int(30)),
            ("3 * 3 * 3 + 10", Object::int(37)),
            ("3 * (3 * 3) + 10", Object::int(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::int(50)),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_minus_operator() -> Result<()> {
        let expectations = vec![
            ("5", Object::int(5)),
            ("10", Object::int(10)),
            ("-5", Object::int(-5)),
            ("-10", Object::int(-10)),
        ];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_boolean_literal() -> Result<()> {
        let expectations = vec![("true", Object::bool(true)), ("false", Object::bool(false))];
        test_from_source(expectations)
    }

    #[test]
    fn test_eval_bang_operator() -> Result<()> {
        let expectations = vec![
            ("!false", Object::bool(true)),
            ("!5", Object::bool(false)),
            ("!!true", Object::bool(true)),
            ("!!false", Object::bool(false)),
            ("!!5", Object::bool(true)),
        ];
        test_from_source(expectations)
    }

    fn test_from_source_err(input: Vec<(&str, &str)>) {
        for (src, expected) in input {
            let mut env = Environment::new();
            let l = Lexer::new(src);
            let mut p = Parser::new(l);
            let err = p
                .parse_program()
                .unwrap()
                .eval(&mut env)
                .expect_err(&format!("{} did not error as expected", src))
                .to_string();
            assert_eq!(
                err, expected,
                "src {} did not have an error message matching {}",
                src, expected
            );
        }
    }

    fn test_from_source(input: Vec<(&str, Object)>) -> anyhow::Result<()> {
        for (src, result) in input {
            let mut env = Environment::new();
            let l = Lexer::new(src);
            let mut p = Parser::new(l);
            assert_eq!(
                p.parse_program().unwrap().eval(&mut env)?,
                result,
                "src {} was not equal to result {}",
                src,
                result
            );
        }
        Ok(())
    }
}
