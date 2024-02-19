use crate::ast::{
    Expression, InfixExpression, InfixOperator, IntegerLiteral, Node, PrefixExpression, Program,
    Statement,
};
use crate::object::{Boolean, Integer, Object};

pub trait Eval {
    fn eval<'a>(&'a self) -> Object;
}

impl Eval for PrefixExpression {
    fn eval<'a>(&'a self) -> Object {
        match (&self.operator, self.right.eval()) {
            (crate::ast::PrefixOperator::Minus, Object::Int(Integer { value })) => {
                // todo: implicit clone :(
                Object::Int(Integer { value: -value })
            }
            (crate::ast::PrefixOperator::Bang, Object::Bool(Boolean::True)) => {
                Object::Bool(Boolean::False)
            }
            (crate::ast::PrefixOperator::Bang, Object::Bool(Boolean::False)) => {
                Object::Bool(Boolean::True)
            }
            (crate::ast::PrefixOperator::Bang, Object::Null) => Object::Bool(Boolean::True),
            (crate::ast::PrefixOperator::Bang, _) => Object::Bool(Boolean::False),
            // todo: improve errors! no null
            (crate::ast::PrefixOperator::Minus, _) => Object::Null,
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
fn eval_infix_booleans(left: Boolean, op: &InfixOperator, right: Boolean) -> Object {
    let left_bool: bool = left.into();
    let right_bool: bool = right.into();
    match op {
        InfixOperator::Gt => Object::bool(left_bool > right_bool),
        InfixOperator::Lt => Object::bool(left_bool < right_bool),
        InfixOperator::Eq => Object::bool(left_bool == right_bool),
        InfixOperator::NotEq => Object::bool(left_bool != right_bool),
        _ => Object::Null,
    }
}
impl Eval for InfixExpression {
    fn eval<'a>(&'a self) -> Object {
        match (self.left.eval(), &self.operator, self.right.eval()) {
            (Object::Int(left), op, Object::Int(right)) => eval_infix_integers(left, op, right),
            (Object::Bool(left), op, Object::Bool(right)) => eval_infix_booleans(left, op, right),
            _ => {
                // todo: no null
                Object::Null
            }
        }
    }
}

// todo: Can we derive eval if the constituants implement eval? :think:
impl Eval for Expression {
    fn eval<'a>(self: &'a Self) -> Object {
        match self {
            Expression::PrefixExpression(prefix_expr) => prefix_expr.eval(),
            Expression::InfixExpression(infix_expr) => infix_expr.eval(),
            Expression::Identifier(_) => todo!(),
            Expression::Boolean(crate::ast::Boolean { value: true }) => Object::Bool(Boolean::True),
            Expression::Boolean(crate::ast::Boolean { value: false }) => {
                Object::Bool(Boolean::False)
            }
            Expression::IntegerLiteral(literal) => Object::Int(Integer {
                value: literal.value,
            }),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::IfExpression(_) => todo!(),
        }
    }
}

impl Eval for Node {
    fn eval<'a>(&'a self) -> Object {
        match self {
            Node::Statement(_stmt) => todo!("stmnt"),
            Node::Expression(expr) => expr.eval(),
        }
    }
}

impl Eval for Statement {
    fn eval<'a>(&'a self) -> Object {
        match self {
            Statement::LetStatement(let_stmt) => todo!(),
            Statement::ReturnStatement(return_stmt) => todo!(),
            Statement::ExpressionStatement(expr_stmt) => expr_stmt.eval(),
        }
    }
}

impl Eval for Program {
    fn eval<'a>(&'a self) -> Object {
        let mut result = Object::Null;
        for stmt in &self.statements {
            result = stmt.eval();
        }
        result
    }
}

#[cfg(test)]
mod tests {

    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_eval_boolean_literal() {
        let expect_false =
            Node::Expression(Expression::Boolean(crate::ast::Boolean { value: false })).eval();
        let expect_true =
            Node::Expression(Expression::Boolean(crate::ast::Boolean { value: true })).eval();
        assert_eq!(expect_false, Object::bool(false));
        assert_eq!(expect_true, Object::bool(true));
    }

    fn test_source_strings(input: Vec<(&str, Object)>) {
        for (src, result) in input {
            let l = Lexer::new(src);
            let mut p = Parser::new(l);
            assert_eq!(
                p.parse_program().unwrap().eval(),
                result,
                "src {} was not equal to result {}",
                src,
                result
            );
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
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
        ];
        test_source_strings(expectations);
    }

    #[test]
    fn test_eval_integer_expression() {
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
        test_source_strings(expectations);
    }

    #[test]
    fn test_eval_minus_operator() {
        let expectations = vec![
            ("5", Object::int(5)),
            ("10", Object::int(10)),
            ("-5", Object::int(-5)),
            ("-10", Object::int(-10)),
        ];
        test_source_strings(expectations);
    }

    #[test]
    fn test_eval_bang_operator() {
        let expectations = vec![
            ("!false", Object::bool(true)),
            ("!5", Object::bool(false)),
            ("!!true", Object::bool(true)),
            ("!!false", Object::bool(false)),
            ("!!5", Object::bool(true)),
        ];
        test_source_strings(expectations);
    }

    #[test]
    fn test_eval_integer_literal() {
        let expr_one = Node::Expression(Expression::IntegerLiteral(IntegerLiteral { value: 5 }));
        let expr_two = Node::Expression(Expression::IntegerLiteral(IntegerLiteral { value: 10 }));
        let expect_5 = expr_one.eval();
        let expect_10 = expr_two.eval();
        assert_eq!(expect_5, Object::Int(Integer { value: 5 }));
        assert_eq!(expect_10, Object::Int(Integer { value: 10 }));
    }
}
