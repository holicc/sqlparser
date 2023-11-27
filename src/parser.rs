use crate::{
    ast::{self, Expression, Statement},
    error::{Error, Result},
    lexer::Lexer,
    token::{Keyword, Token, TokenType},
};
use std::iter::Peekable;

pub struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    pub fn new(sql: String) -> Parser {
        Parser {
            lexer: Lexer::new(sql).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Statement> {
        match self.lexer.next() {
            Some(token) => match token.token_type {
                TokenType::Keyword(Keyword::Select) => self.parse_select_statement(),
                _ => Err(Error::UnexpectedToken(token)),
            },
            None => Err(Error::UnexpectedEOF),
        }
    }

    fn parse_select_statement(&mut self) -> Result<Statement> {
        let columns = self.parse_columns()?;

        let from = self.parse_from_statment()?;

        Ok(Statement::Select {
            distinct: None,
            columns,
            from,
            // TODO
            r#where: None,
            group_by: None,
        })
    }

    fn parse_columns(&mut self) -> Result<Vec<(ast::Expression, Option<String>)>> {
        let mut columns = Vec::new();

        loop {
            let expr = self.parse_expression()?;
            let alias = if self
                .next_if_token(TokenType::Keyword(Keyword::As))
                .is_some()
            {
                Some(self.parse_expression()?.to_string())
            } else {
                None
            };
            columns.push((expr, alias));

            if self.next_if_token(TokenType::Comma).is_none() {
                break;
            }
        }

        Ok(columns)
    }

    fn parse_from_statment(&mut self) -> Result<ast::From> {
        let token = self.lexer.next().ok_or(Error::UnexpectedEOF)?;
        if token.token_type != TokenType::Keyword(Keyword::From) {
            return Err(Error::UnexpectedToken(token));
        }

        Ok(ast::From::Table {
            name: self.parse_expression()?.to_string(),
            alias: None,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        if let Some(prefix) = self.next_if_operator::<PrefixOperator>() {
            return Ok(prefix.build(self.parse_expression()?));
        }

        match self.lexer.next().ok_or(Error::UnexpectedEOF)? {
            Token {
                token_type: TokenType::Ident,
                literal,
            } => Ok(ast::Expression::Literal(ast::Literal::String(literal))),
            Token {
                token_type: TokenType::Asterisk,
                ..
            } => Ok(ast::Expression::Literal(ast::Literal::String(
                "*".to_owned(),
            ))),
            Token {
                token_type: TokenType::Int,
                literal,
            } => Ok(ast::Expression::Literal(ast::Literal::Int(
                literal.parse().map_err(|e| Error::ParseIntError(e))?,
            ))),
            token => Err(Error::UnexpectedToken(token)),
        }
    }

    fn next_if_operator<O: Operator>(&mut self) -> Option<O> {
        self.lexer.peek().filter(|t| match t.token_type {
            TokenType::Plus | TokenType::Minus | TokenType::Bang => true,
            _ => false,
        })?;
        O::from(&self.lexer.next()?)
    }

    fn next_if_token(&mut self, token: TokenType) -> Option<Token> {
        self.lexer.peek().filter(|t| t.token_type == token)?;
        self.lexer.next()
    }
}

trait Operator: Sized {
    fn from(token: &Token) -> Option<Self>;
}

enum PrefixOperator {
    Plus,
    Minus,
    Not,
}

impl Operator for PrefixOperator {
    fn from(token: &Token) -> Option<Self> {
        match token.token_type {
            TokenType::Plus => Some(PrefixOperator::Plus),
            TokenType::Minus => Some(PrefixOperator::Minus),
            TokenType::Bang => Some(PrefixOperator::Not),
            _ => None,
        }
    }
}

impl PrefixOperator {
    fn build(&self, rhs: Expression) -> Expression {
        match self {
            PrefixOperator::Plus => Expression::Operator(ast::Operator::Pos(Box::new(rhs))),
            PrefixOperator::Minus => Expression::Operator(ast::Operator::Neg(Box::new(rhs))),
            PrefixOperator::Not => Expression::Operator(ast::Operator::Not(Box::new(rhs))),
        }
    }
}

enum InfixOperator {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    NotEq,
    And,
    Or,
}

impl Operator for InfixOperator{
    fn from(token: &Token) -> Option<Self> {
        match token.token_type{
            TokenType::Plus => Some(InfixOperator::Add),
            TokenType::Minus => Some(InfixOperator::Sub),
            TokenType::Asterisk => Some(InfixOperator::Mul),
            TokenType::Slash => Some(InfixOperator::Div),
            TokenType::Gt => Some(InfixOperator::Gt),
            TokenType::Gte => Some(InfixOperator::Gte),
            TokenType::Lt => Some(InfixOperator::Lt),
            TokenType::Lte => Some(InfixOperator::Lte),
            TokenType::Eq => Some(InfixOperator::Eq),
            TokenType::NotEq => Some(InfixOperator::NotEq),
            TokenType::And => Some(InfixOperator::And),
            TokenType::Or => Some(InfixOperator::Or),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::{self, Expression, Statement};
    use crate::error::Result;

    #[test]
    fn test_parse_select_statement() {
        let stmt = parse_stmt("SELECT * FROM users;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                },
                r#where: None,
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_ident() {
        let stmt = parse_expr("foobar").unwrap();

        assert_eq!(
            stmt,
            Expression::Literal(ast::Literal::String("foobar".to_owned()))
        );
    }

    #[test]
    fn test_parse_integer() {
        let stmt = parse_expr("123").unwrap();

        assert_eq!(stmt, Expression::Literal(ast::Literal::Int(123)));
    }

    #[test]
    fn test_parse_prefix_expression() {
        let stmt = parse_expr("-123").unwrap();

        assert_eq!(
            stmt,
            Expression::Operator(ast::Operator::Neg(Box::new(Expression::Literal(
                ast::Literal::Int(123)
            ))))
        );
    }

    #[test]
    fn test_parse_infix_expression() {
        let tests = vec![
            (
                "1 + 2",
                Expression::Operator(ast::Operator::Add(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(2))),
                )),
            ),
            (
                "1 - 2",
                Expression::Operator(ast::Operator::Sub(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(2))),
                )),
            ),
            (
                "1 / 1",
                Expression::Operator(ast::Operator::Div(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 * 5",
                Expression::Operator(ast::Operator::Mul(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(5))),
                )),
            ),
            (
                "1 = 1",
                Expression::Operator(ast::Operator::Eq(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 != 1",
                Expression::Operator(ast::Operator::NotEq(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 > 1",
                Expression::Operator(ast::Operator::Gt(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 >= 1",
                Expression::Operator(ast::Operator::Gte(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 < 1",
                Expression::Operator(ast::Operator::Lt(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 <= 1",
                Expression::Operator(ast::Operator::Lte(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 AND 1",
                Expression::Operator(ast::Operator::And(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
            (
                "1 OR 1",
                Expression::Operator(ast::Operator::Or(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                )),
            ),
        ];

        for test in tests {
            assert_eq!(parse_expr(test.0).unwrap(), test.1)
        }
    }

    fn parse_stmt(input: &str) -> Result<Statement> {
        let mut parser = Parser::new(input.to_owned());
        parser.parse()
    }

    fn parse_expr(input: &str) -> Result<Expression> {
        let mut parser = Parser::new(input.to_owned());
        parser.parse_expression()
    }
}
