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
            Some(Token {
                token_type: TokenType::Keyword(Keyword::Select),
                ..
            }) => self.parse_select_statement(),
            _ => Err(Error::UnexpectedEOF),
        }
    }

    fn parse_select_statement(&mut self) -> Result<Statement> {
        let distinct = self.parse_distinct()?;

        let columns = self.parse_columns()?;

        if self
            .next_if_token(TokenType::Keyword(Keyword::From))
            .is_none()
        {
            return Err(Error::UnexpectedEOF);
        }
        let from = self.parse_from_statment()?;

        Ok(Statement::Select {
            distinct,
            columns,
            from,
            // TODO
            r#where: None,
            group_by: None,
        })
    }

    fn parse_distinct(&mut self) -> Result<Option<ast::Distinct>> {
        if self
            .next_if_token(TokenType::Keyword(Keyword::Distinct))
            .is_some()
        {
            if self
                .next_if_token(TokenType::Keyword(Keyword::On))
                .is_some()
            {
                self.next_if_token(TokenType::LParen)
                    .ok_or(Error::UnexpectedEOF)?;

                let mut columns = Vec::new();
                while self.next_if_token(TokenType::RParen).is_none() {
                    columns.push(self.parse_expression(0)?);
                    self.next_if_token(TokenType::Comma);
                }

                Ok(Some(ast::Distinct::DISTINCT(columns)))
            } else {
                Ok(Some(ast::Distinct::ALL))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_columns(&mut self) -> Result<Vec<(ast::Expression, Option<String>)>> {
        let mut columns = Vec::new();

        loop {
            if self.next_if_token(TokenType::Comma).is_some() {
                continue;
            }

            let expr = self.parse_expression(0)?;
            let alias = if self
                .next_if_token(TokenType::Keyword(Keyword::As))
                .is_some()
            {
                Some(self.parse_expression(0)?.to_string())
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
        // parse subquery
        if self.next_if_token(TokenType::LParen).is_some() {
            if self
                .next_if_token(TokenType::Keyword(Keyword::Select))
                .is_none()
            {
                return Err(Error::UnexpectedEOF);
            }

            let subquery = self.parse_select_statement()?;

            self.next_if_token(TokenType::RParen)
                .ok_or(Error::UnexpectedEOF)?;

            return Ok(ast::From::SubQuery {
                query: Box::new(subquery),
                alias: self.parse_alias(),
            });
        }

        // parse table refereneces
        let table_ref = self.parse_table_reference()?;

        // parse join cause
        if let Some(join_type) = self.parse_join_type()? {
            let right = self.parse_from_statment()?;
            let on = if join_type == ast::JoinType::Cross {
                None
            } else {
                if self
                    .next_if_token(TokenType::Keyword(Keyword::On))
                    .is_none()
                {
                    return Err(Error::UnexpectedEOF);
                }
                Some(self.parse_expression(0)?)
            };

            return Ok(ast::From::Join {
                join_type,
                left: Box::new(table_ref),
                right: Box::new(right),
                on,
            });
        }

        Ok(table_ref)
    }

    fn parse_join_type(&mut self) -> Result<Option<ast::JoinType>> {
        if self
            .next_if_token(TokenType::Keyword(Keyword::Join))
            .is_some()
        {
            Ok(Some(ast::JoinType::Inner))
        } else if self
            .next_if_token(TokenType::Keyword(Keyword::Left))
            .is_some()
        {
            self.next_if_token(TokenType::Keyword(Keyword::Join));
            Ok(Some(ast::JoinType::Left))
        } else if self
            .next_if_token(TokenType::Keyword(Keyword::Right))
            .is_some()
        {
            self.next_if_token(TokenType::Keyword(Keyword::Join));
            Ok(Some(ast::JoinType::Right))
        } else if self
            .next_if_token(TokenType::Keyword(Keyword::Full))
            .is_some()
        {
            self.next_if_token(TokenType::Keyword(Keyword::Join));
            Ok(Some(ast::JoinType::Full))
        } else if self
            .next_if_token(TokenType::Keyword(Keyword::Cross))
            .is_some()
        {
            self.next_if_token(TokenType::Keyword(Keyword::Join));
            Ok(Some(ast::JoinType::Cross))
        } else {
            Ok(None)
        }
    }

    fn parse_table_reference(&mut self) -> Result<ast::From> {
        let mut table_name = self.next_ident().ok_or(Error::UnexpectedEOF)?;
        let mut is_table_function = false;
        let mut args = Vec::new();

        while let Some(preiod) = self.next_if_token(TokenType::Period) {
            table_name.push_str(&preiod.literal);
            table_name.push_str(&self.next_ident().ok_or(Error::UnexpectedEOF)?);
        }

        // parse table function
        if self.next_if_token(TokenType::LParen).is_some() {
            is_table_function = true;
            while self.next_if_token(TokenType::RParen).is_none() {
                args.push(self.parse_expression(0)?);
                self.next_if_token(TokenType::Comma);
            }
        }

        let alias = self.parse_alias();

        if is_table_function {
            return Ok(ast::From::TableFunction {
                name: table_name,
                args,
                alias,
            });
        }

        Ok(ast::From::Table {
            name: table_name,
            alias,
        })
    }

    fn parse_alias(&mut self) -> Option<String> {
        if self
            .next_if_token(TokenType::Keyword(Keyword::As))
            .is_some()
        {
            self.next_ident()
        } else if let Some(ident) = self.next_ident() {
            Some(ident)
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression> {
        let mut lhs = if let Some(prefix) = self.next_if_operator::<PrefixOperator>(precedence) {
            prefix.build(self.parse_expression(prefix.precedence())?)
        } else {
            self.parse_expression_atom()?
        };

        while let Some(infix) = self.next_if_operator::<InfixOperator>(precedence) {
            if infix.precedence() < precedence {
                break;
            }
            lhs = infix.build(lhs, self.parse_expression(infix.precedence())?);
        }

        Ok(lhs)
    }

    fn parse_expression_atom(&mut self) -> Result<Expression> {
        match self.lexer.next().ok_or(Error::UnexpectedEOF)? {
            Token {
                token_type: TokenType::Ident,
                mut literal,
            } => {
                // parse function
                if self.next_if_token(TokenType::LParen).is_some() {
                    let mut args = Vec::new();
                    while self.next_if_token(TokenType::RParen).is_none() {
                        args.push(self.parse_expression(0)?);
                        self.next_if_token(TokenType::Comma);
                    }
                    Ok(ast::Expression::Function(literal, args))
                } else {
                    while let Some(p) = self.next_if_token(TokenType::Period) {
                        literal.push_str(&p.literal);
                        literal.push_str(&self.next_ident().ok_or(Error::UnexpectedEOF)?);
                    }
                    Ok(ast::Expression::Literal(ast::Literal::String(literal)))
                }
            }
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
            Token {
                token_type: TokenType::String,
                literal,
            } => Ok(ast::Expression::Literal(ast::Literal::String(literal))),
            Token {
                token_type: TokenType::Keyword(Keyword::True),
                ..
            } => Ok(ast::Expression::Literal(ast::Literal::Boolean(true))),
            Token {
                token_type: TokenType::Keyword(Keyword::False),
                ..
            } => Ok(ast::Expression::Literal(ast::Literal::Boolean(false))),
            Token {
                token_type: TokenType::LParen,
                ..
            } => {
                let expr = self.parse_expression(0)?;
                self.next_if_token(TokenType::RParen)
                    .ok_or(Error::UnexpectedEOF)?;
                Ok(expr)
            }
            token => Err(Error::UnexpectedToken(token)),
        }
    }

    fn next_ident(&mut self) -> Option<String> {
        self.lexer
            .peek()
            .filter(|t| t.token_type == TokenType::Ident)?;
        self.lexer.next().map(|t| t.literal)
    }

    fn next_if_operator<O: Operator>(&mut self, precedence: u8) -> Option<O> {
        self.lexer
            .peek()
            .and_then(|t| O::from(t))
            .filter(|op| op.precedence() >= precedence)?;
        O::from(&self.lexer.next()?)
    }

    fn next_if_token(&mut self, token: TokenType) -> Option<Token> {
        self.lexer.peek().filter(|t| t.token_type == token)?;
        self.lexer.next()
    }
}

trait Operator: Sized {
    fn from(token: &Token) -> Option<Self>;

    fn precedence(&self) -> u8;
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

    fn precedence(&self) -> u8 {
        9
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

#[derive(Debug)]
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

impl Operator for InfixOperator {
    fn from(token: &Token) -> Option<Self> {
        match token.token_type {
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
            TokenType::Keyword(Keyword::And) => Some(InfixOperator::And),
            TokenType::Keyword(Keyword::Or) => Some(InfixOperator::Or),
            _ => None,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            InfixOperator::Or => 1,
            InfixOperator::And => 2,
            InfixOperator::Eq | InfixOperator::NotEq => 3,
            InfixOperator::Gt | InfixOperator::Gte | InfixOperator::Lt | InfixOperator::Lte => 4,
            InfixOperator::Add | InfixOperator::Sub => 5,
            InfixOperator::Mul | InfixOperator::Div => 6,
        }
    }
}

impl InfixOperator {
    pub fn build(&self, lhr: Expression, rhs: Expression) -> Expression {
        match self {
            InfixOperator::Add => {
                Expression::Operator(ast::Operator::Add(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Sub => {
                Expression::Operator(ast::Operator::Sub(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Mul => {
                Expression::Operator(ast::Operator::Mul(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Div => {
                Expression::Operator(ast::Operator::Div(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Gt => {
                Expression::Operator(ast::Operator::Gt(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Gte => {
                Expression::Operator(ast::Operator::Gte(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Lt => {
                Expression::Operator(ast::Operator::Lt(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Lte => {
                Expression::Operator(ast::Operator::Lte(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Eq => {
                Expression::Operator(ast::Operator::Eq(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::NotEq => {
                Expression::Operator(ast::Operator::NotEq(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::And => {
                Expression::Operator(ast::Operator::And(Box::new(lhr), Box::new(rhs)))
            }
            InfixOperator::Or => {
                Expression::Operator(ast::Operator::Or(Box::new(lhr), Box::new(rhs)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

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
    fn test_parse_from_item() {
        let stmt = parse_stmt("select * from public.users as u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Table {
                    name: String::from("public.users"),
                    alias: Some(String::from("u")),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from catalog.public.users u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Table {
                    name: String::from("catalog.public.users"),
                    alias: Some(String::from("u")),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from catalog.public.users(1,'2',box(1));").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::TableFunction {
                    name: String::from("catalog.public.users"),
                    args: vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("2".to_owned())),
                        ast::Expression::Function(
                            String::from("box"),
                            vec![ast::Expression::Literal(ast::Literal::Int(1))]
                        ),
                    ],
                    alias: None,
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from (select * from users) as u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::SubQuery {
                    query: Box::new(ast::Statement::Select {
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
                    }),
                    alias: Some(String::from("u")),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Join {
                    join_type: ast::JoinType::Inner,
                    left: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u")),
                    }),
                    right: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u2")),
                    }),
                    on: Some(ast::Expression::Operator(ast::Operator::Eq(
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u.id".to_owned()
                        ))),
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u2.id".to_owned()
                        ))),
                    ))),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u left join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Join {
                    join_type: ast::JoinType::Left,
                    left: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u")),
                    }),
                    right: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u2")),
                    }),
                    on: Some(ast::Expression::Operator(ast::Operator::Eq(
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u.id".to_owned()
                        ))),
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u2.id".to_owned()
                        ))),
                    ))),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt =
            parse_stmt("select * from users u right join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Join {
                    join_type: ast::JoinType::Right,
                    left: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u")),
                    }),
                    right: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u2")),
                    }),
                    on: Some(ast::Expression::Operator(ast::Operator::Eq(
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u.id".to_owned()
                        ))),
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u2.id".to_owned()
                        ))),
                    ))),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u full join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Join {
                    join_type: ast::JoinType::Full,
                    left: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u")),
                    }),
                    right: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u2")),
                    }),
                    on: Some(ast::Expression::Operator(ast::Operator::Eq(
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u.id".to_owned()
                        ))),
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "u2.id".to_owned()
                        ))),
                    ))),
                },
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u cross join users u2;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: ast::From::Join {
                    join_type: ast::JoinType::Cross,
                    left: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u")),
                    }),
                    right: Box::new(ast::From::Table {
                        name: String::from("users"),
                        alias: Some(String::from("u2")),
                    }),
                    on: None,
                },
                r#where: None,
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_distinct_select_statement() {
        let stmt = parse_stmt("SELECT DISTINCT * FROM users;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: Some(ast::Distinct::ALL),
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

        let stmt = parse_stmt("SELECT DISTINCT ON(name,age),school FROM users;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: Some(ast::Distinct::DISTINCT(vec![
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("age".to_owned())),
                ])),
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("school".to_owned())),
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
    fn test_parse_boolean() {
        let stmt = parse_expr("true").unwrap();

        assert_eq!(stmt, Expression::Literal(ast::Literal::Boolean(true)));

        let stmt = parse_expr("false").unwrap();

        assert_eq!(stmt, Expression::Literal(ast::Literal::Boolean(false)));
    }

    #[test]
    fn test_parse_function() {
        let stmt = parse_expr("foo(1, 2, 3)").unwrap();

        assert_eq!(
            stmt,
            Expression::Function(
                "foo".to_owned(),
                vec![
                    Expression::Literal(ast::Literal::Int(1)),
                    Expression::Literal(ast::Literal::Int(2)),
                    Expression::Literal(ast::Literal::Int(3)),
                ]
            )
        );

        let stmt = parse_expr("foo(bar(1, 2, 3))").unwrap();

        assert_eq!(
            stmt,
            Expression::Function(
                "foo".to_owned(),
                vec![Expression::Function(
                    "bar".to_owned(),
                    vec![
                        Expression::Literal(ast::Literal::Int(1)),
                        Expression::Literal(ast::Literal::Int(2)),
                        Expression::Literal(ast::Literal::Int(3)),
                    ]
                ),]
            )
        );
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
            (
                "-a * b",
                Expression::Operator(ast::Operator::Mul(
                    Box::new(Expression::Operator(ast::Operator::Neg(Box::new(
                        Expression::Literal(ast::Literal::String("a".to_owned())),
                    )))),
                    Box::new(Expression::Literal(ast::Literal::String("b".to_owned()))),
                )),
            ),
            (
                "a + b * c",
                Expression::Operator(ast::Operator::Add(
                    Box::new(Expression::Literal(ast::Literal::String("a".to_owned()))),
                    Box::new(Expression::Operator(ast::Operator::Mul(
                        Box::new(Expression::Literal(ast::Literal::String("b".to_owned()))),
                        Box::new(Expression::Literal(ast::Literal::String("c".to_owned()))),
                    ))),
                )),
            ),
            (
                "5 > 1 AND 3 < 4",
                Expression::Operator(ast::Operator::And(
                    Box::new(Expression::Operator(ast::Operator::Gt(
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                        Box::new(Expression::Literal(ast::Literal::Int(1))),
                    ))),
                    Box::new(Expression::Operator(ast::Operator::Lt(
                        Box::new(Expression::Literal(ast::Literal::Int(3))),
                        Box::new(Expression::Literal(ast::Literal::Int(4))),
                    ))),
                )),
            ),
            (
                "1 + (2 + 3) + 4",
                Expression::Operator(ast::Operator::Add(
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                    Box::new(Expression::Operator(ast::Operator::Add(
                        Box::new(Expression::Operator(ast::Operator::Add(
                            Box::new(Expression::Literal(ast::Literal::Int(2))),
                            Box::new(Expression::Literal(ast::Literal::Int(3))),
                        ))),
                        Box::new(Expression::Literal(ast::Literal::Int(4))),
                    ))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Expression::Operator(ast::Operator::Mul(
                    Box::new(Expression::Operator(ast::Operator::Add(
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                    ))),
                    Box::new(Expression::Literal(ast::Literal::Int(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Expression::Operator(ast::Operator::Div(
                    Box::new(Expression::Literal(ast::Literal::Int(2))),
                    Box::new(Expression::Operator(ast::Operator::Add(
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                    ))),
                )),
            ),
            (
                "-(5 + 5)",
                Expression::Operator(ast::Operator::Neg(Box::new(Expression::Operator(
                    ast::Operator::Add(
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                        Box::new(Expression::Literal(ast::Literal::Int(5))),
                    ),
                )))),
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
        parser.parse_expression(0)
    }
}
