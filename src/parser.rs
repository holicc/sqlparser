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
        let distinct = if self.next_if_token(TokenType::Keyword(Keyword::Distinct)) {
            Some(self.parse_distinct_statment()?)
        } else {
            None
        };

        let columns = self.parse_columns()?;

        let from = self.parse_from_statment()?;

        let r#where = if self.next_if_token(TokenType::Keyword(Keyword::Where)) {
            // TODO
            todo!()
        } else {
            None
        };

        let group_by = if self.next_if_token(TokenType::Keyword(Keyword::Group)) {
            // TODO
            todo!()
        } else {
            None
        };

        Ok(Statement::Select {
            distinct,
            columns,
            from,
            // TODO
            r#where,
            group_by,
        })
    }

    fn next_if_token(&mut self, token: TokenType) -> bool {
        if let Some(t) = self.lexer.peek() {
            if t.token_type == token {
                self.lexer.next();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn parse_columns(&mut self) -> Result<Vec<(ast::Expression, Option<String>)>> {
        let mut columns = Vec::new();

        loop {
            let expr = self.parse_expression()?;

            // TODO parse alias
            columns.push((expr, None));

            if !self.next_if_token(TokenType::Comma) {
                break;
            }
        }

        Ok(columns)
    }

    fn parse_distinct_statment(&mut self) -> Result<ast::Distinct> {
        todo!()
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
            token => Err(Error::UnexpectedToken(token)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast;

    use super::Parser;

    #[test]
    fn test_parse_select_statement() {
        let sql = String::from("SELECT * FROM users;");
        let mut parser = Parser::new(sql);

        let stmt = parser.parse().unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None
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
}
