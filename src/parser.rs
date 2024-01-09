use crate::{
    ast::{self, Expression, OnConflict, Order, Statement},
    error::{Error, Result},
    lexer::Lexer,
    token::{Keyword, Token, TokenType},
};
use std::{collections::BTreeMap, iter::Peekable};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(sql: &'a str) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(sql).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Statement> {
        let token = self.lexer.next().ok_or(Error::UnexpectedEOF)?;

        match token.token_type {
            TokenType::Keyword(Keyword::Select) => self.parse_select_statement(),
            TokenType::Keyword(Keyword::Insert) => self.parse_insert_statement(),
            TokenType::Keyword(Keyword::Update) => self.parse_update_statement(),
            TokenType::Keyword(Keyword::Delete) => self.parse_delete_statement(),
            TokenType::Keyword(Keyword::Create) => self.parse_create_statement(),
            TokenType::Keyword(Keyword::Drop) => self.parse_drop_statement(),
            _ => Err(Error::UnexpectedToken(token)),
        }
    }

    fn parse_drop_statement(&mut self) -> Result<Statement> {
        match self.lexer.next().ok_or(Error::UnexpectedEOF)?.token_type {
            TokenType::Keyword(Keyword::Schema) => {
                let check_exists = self.parse_if_exists()?;
                let schema = self.next_ident().ok_or(Error::UnexpectedEOF)?;

                Ok(Statement::DropSchema {
                    schema,
                    check_exists,
                })
            }
            TokenType::Keyword(Keyword::Table) => {
                let check_exists = self.parse_if_exists()?;
                let table = self.next_ident().ok_or(Error::UnexpectedEOF)?;

                Ok(Statement::DropTable {
                    table,
                    check_exists,
                })
            }
            _ => unimplemented!(),
        }
    }

    fn parse_create_statement(&mut self) -> Result<Statement> {
        match self.lexer.next().ok_or(Error::UnexpectedEOF)?.token_type {
            TokenType::Keyword(Keyword::Schema) => self.parse_create_schema(),
            TokenType::Keyword(Keyword::Table) => self.parse_create_table(),
            _ => unimplemented!(),
        }
    }

    fn parse_create_table(&mut self) -> Result<Statement> {
        let check_exists = self.parse_if_not_exists()?;
        let table = self.next_ident().ok_or(Error::UnexpectedEOF)?;
        let mut columns = Vec::new();
        // parse table columns
        if self.next_if_token(TokenType::LParen).is_some() {
            loop {
                if self.next_if_token(TokenType::RParen).is_some() {
                    break;
                }

                let name = self.next_ident().ok_or(Error::UnexpectedEOF)?;
                let mut nullable = true;
                let datatype = self
                    .lexer
                    .next()
                    .ok_or(Error::UnexpectedEOF)
                    .and_then(|t| t.datatype())?;

                let primary_key = if self
                    .next_if_token(TokenType::Keyword(Keyword::Primary))
                    .is_some()
                {
                    self.next_if_token(TokenType::Keyword(Keyword::Key))
                        .ok_or(Error::UnexpectedEOF)?;
                    nullable = false;
                    true
                } else {
                    false
                };

                let unique = self
                    .next_if_token(TokenType::Keyword(Keyword::Unique))
                    .is_some();

                if self
                    .next_if_token(TokenType::Keyword(Keyword::Not))
                    .is_some()
                {
                    self.next_if_token(TokenType::Keyword(Keyword::Null))
                        .ok_or(Error::UnexpectedEOF)?;
                    nullable = false;
                }

                columns.push(ast::Column {
                    name,
                    datatype,
                    nullable,
                    unique,
                    references: None,
                    primary_key,
                    index: false,
                });

                if self.next_if_token(TokenType::Comma).is_none() {
                    break;
                }
            }
        }

        Ok(Statement::CreateTable {
            table,
            columns,
            check_exists,
        })
    }

    fn parse_create_schema(&mut self) -> Result<Statement> {
        let check_exists: bool = self.parse_if_not_exists()?;
        let schema = self.next_ident().ok_or(Error::UnexpectedEOF)?;

        Ok(Statement::CreateSchema {
            schema,
            check_exists,
        })
    }

    fn parse_delete_statement(&mut self) -> Result<Statement> {
        self.next_if_token(TokenType::Keyword(Keyword::From))
            .ok_or(Error::UnexpectedEOF)?;

        let table = self.next_ident().ok_or(Error::UnexpectedEOF)?;

        let r#where = if self
            .next_if_token(TokenType::Keyword(Keyword::Where))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        Ok(Statement::Delete { table, r#where })
    }

    fn parse_update_statement(&mut self) -> Result<Statement> {
        let table = self.next_ident().ok_or(Error::UnexpectedEOF)?;

        self.next_except(TokenType::Keyword(Keyword::Set))?;

        let mut assignments = BTreeMap::new();
        loop {
            let column = self.next_ident().ok_or(Error::UnexpectedEOF)?;

            self.next_except(TokenType::Eq)?;

            let value = self.parse_expression(0)?;

            if assignments.contains_key(&column) {
                return Err(Error::DuplicateColumn(column));
            }

            assignments.insert(column, value);

            if self.next_if_token(TokenType::Comma).is_none() {
                break;
            }
        }

        let r#where = if self
            .next_if_token(TokenType::Keyword(Keyword::Where))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        Ok(Statement::Update {
            table,
            assignments,
            r#where,
        })
    }

    fn parse_insert_statement(&mut self) -> Result<Statement> {
        self.next_if_token(TokenType::Keyword(Keyword::Into))
            .ok_or(Error::UnexpectedEOF)?;

        let table_name = self.next_ident().ok_or(Error::UnexpectedEOF)?;

        let alias = self.parse_alias();

        let columns = if self.next_if_token(TokenType::LParen).is_some() {
            let columns = self
                .parse_columns()?
                .into_iter()
                .map(|v| v.0)
                .collect::<Vec<_>>();

            self.next_if_token(TokenType::RParen)
                .ok_or(Error::UnexpectedEOF)?;

            Some(columns)
        } else {
            None
        };

        self.next_if_token(TokenType::Keyword(Keyword::Values))
            .ok_or(Error::UnexpectedEOF)?;
        let values = self.parse_values()?;

        let on_conflict = if self
            .next_if_token(TokenType::Keyword(Keyword::On))
            .is_some()
        {
            Some(self.parse_on_conflict()?)
        } else {
            None
        };

        let returning = if self
            .next_if_token(TokenType::Keyword(Keyword::Returning))
            .is_some()
        {
            Some(self.parse_columns()?)
        } else {
            None
        };

        Ok(Statement::Insert {
            table: (table_name, alias),
            columns,
            values,
            on_conflict,
            returning,
        })
    }

    fn parse_select_statement(&mut self) -> Result<Statement> {
        let distinct = self.parse_distinct()?;

        let columns = self.parse_columns()?;

        if self
            .next_if_token(TokenType::Keyword(Keyword::From))
            .is_none()
        {
            return Ok(Statement::Select {
                distinct,
                columns,
                from: None,
                r#where: None,
                group_by: None,
                having: None,
                order_by: None,
                limit: None,
                offset: None,
            });
        }
        let from = self.parse_from_statment()?;

        let r#where = if self
            .next_if_token(TokenType::Keyword(Keyword::Where))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        let group_by = if self
            .next_if_token(TokenType::Keyword(Keyword::Group))
            .is_some()
        {
            Some(self.parse_group_by()?)
        } else {
            None
        };

        let having = if self
            .next_if_token(TokenType::Keyword(Keyword::Having))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        let order_by = if self
            .next_if_token(TokenType::Keyword(Keyword::Order))
            .is_some()
        {
            Some(self.parse_order_by()?)
        } else {
            None
        };

        let limit = if self
            .next_if_token(TokenType::Keyword(Keyword::Limit))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        let offset = if self
            .next_if_token(TokenType::Keyword(Keyword::Offset))
            .is_some()
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        Ok(Statement::Select {
            distinct,
            columns,
            from: Some(from),
            r#where,
            group_by,
            having,
            order_by,
            limit,
            offset,
        })
    }

    fn parse_on_conflict(&mut self) -> Result<OnConflict> {
        self.next_if_token(TokenType::Keyword(Keyword::Conflict))
            .ok_or(Error::UnexpectedEOF)?;

        self.next_if_token(TokenType::LParen)
            .ok_or(Error::UnexpectedEOF)?;

        let columns = self.parse_columns()?.into_iter().map(|v| v.0).collect();

        self.next_if_token(TokenType::RParen)
            .ok_or(Error::UnexpectedEOF)?;

        self.next_if_token(TokenType::Keyword(Keyword::Do))
            .ok_or(Error::UnexpectedEOF)?;

        if self
            .next_if_token(TokenType::Keyword(Keyword::Nothing))
            .is_some()
        {
            Ok(OnConflict::DoNothing)
        } else if self
            .next_if_token(TokenType::Keyword(Keyword::Update))
            .is_some()
        {
            self.next_if_token(TokenType::Keyword(Keyword::Set))
                .ok_or(Error::UnexpectedEOF)?;

            let mut exprs = Vec::new();
            loop {
                exprs.push(self.parse_expression(0)?);
                if self.next_if_token(TokenType::Comma).is_none() {
                    break;
                }
            }

            Ok(OnConflict::DoUpdate {
                constraints: columns,
                values: exprs,
            })
        } else {
            Err(Error::UnexpectedEOF)
        }
    }

    fn parse_values(&mut self) -> Result<Vec<Vec<Expression>>> {
        let mut values = Vec::new();
        loop {
            if self.next_if_token(TokenType::Comma).is_some() {
                continue;
            }
            let mut row = Vec::new();
            self.next_if_token(TokenType::LParen)
                .ok_or(Error::UnexpectedEOF)?;
            while self.next_if_token(TokenType::RParen).is_none() {
                row.push(self.parse_expression(0)?);
                self.next_if_token(TokenType::Comma);
            }
            values.push(row);
            if self.next_if_token(TokenType::Comma).is_none() {
                break;
            }
        }
        Ok(values)
    }

    fn parse_if_not_exists(&mut self) -> Result<bool> {
        let mut check_exists = false;
        if self
            .next_if_token(TokenType::Keyword(Keyword::If))
            .is_some()
        {
            self.next_except(TokenType::Keyword(Keyword::Not))?;
            self.next_except(TokenType::Keyword(Keyword::Exists))?;

            check_exists = true;
        }
        Ok(check_exists)
    }

    fn parse_if_exists(&mut self) -> Result<bool> {
        let mut check_exists = false;
        if self
            .next_if_token(TokenType::Keyword(Keyword::If))
            .is_some()
        {
            self.next_except(TokenType::Keyword(Keyword::Exists))?;

            check_exists = true;
        }
        Ok(check_exists)
    }

    fn parse_order_by(&mut self) -> Result<Vec<(Expression, Order)>> {
        self.next_if_token(TokenType::Keyword(Keyword::By))
            .ok_or(Error::UnexpectedEOF)?;

        let mut order_fields = vec![];
        loop {
            let expr = self.parse_expression(0)?;
            let mut order = ast::Order::Asc;

            if self
                .next_if_token(TokenType::Keyword(Keyword::Desc))
                .is_some()
            {
                order = ast::Order::Desc;
            }

            order_fields.push((expr, order));

            if self.next_if_token(TokenType::Comma).is_none() {
                break;
            }
        }

        Ok(order_fields)
    }

    fn parse_group_by(&mut self) -> Result<Vec<Expression>> {
        self.next_if_token(TokenType::Keyword(Keyword::By))
            .ok_or(Error::UnexpectedEOF)?;

        let mut group_by = Vec::new();
        while self.next_if_token(TokenType::Semicolon).is_none() {
            group_by.push(self.parse_expression(0)?);
            if self.next_if_token(TokenType::Comma).is_none() {
                break;
            }
        }

        Ok(group_by)
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

    fn parse_in_expr(&mut self, lhs: Expression, negated: bool) -> Result<Expression> {
        self.next_if_token(TokenType::LParen)
            .ok_or(Error::UnexpectedEOF)?;

        if self
            .next_if_token(TokenType::Keyword(Keyword::Select))
            .is_some()
        {
            Ok(Expression::InSubQuery {
                field: Box::new(lhs),
                query: Box::new(self.parse_select_statement()?),
                negated,
            })
        } else {
            let mut list = Vec::new();
            while self.next_if_token(TokenType::RParen).is_none() {
                list.push(self.parse_expression(0)?);
                self.next_if_token(TokenType::Comma);
            }
            Ok(Expression::InList {
                field: Box::new(lhs),
                list,
                negated,
            })
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Expression> {
        let mut lhs = if let Some(prefix) = self.next_if_operator::<PrefixOperator>(precedence) {
            prefix.build(self.parse_expression(prefix.precedence())?)
        } else {
            self.parse_expression_atom()?
        };

        let negated = self
            .next_if_token(TokenType::Keyword(Keyword::Not))
            .is_some();

        while let Some(infix) = self.next_if_operator::<InfixOperator>(precedence) {
            if infix.precedence() < precedence {
                break;
            }
            lhs = match infix {
                InfixOperator::In => self.parse_in_expr(lhs, negated)?,
                _ => infix.build(lhs, self.parse_expression(infix.precedence())?)?,
            }
        }

        Ok(lhs)
    }

    fn parse_expression_atom(&mut self) -> Result<Expression> {
        match self.lexer.next().ok_or(Error::UnexpectedEOF)? {
            Token {
                token_type: TokenType::Ident,
                mut literal,
                ..
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
                ..
            } => Ok(ast::Expression::Literal(ast::Literal::Int(
                literal.parse().map_err(|e| Error::ParseIntError(e))?,
            ))),
            Token {
                token_type: TokenType::String,
                literal,
                ..
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

    fn next_except(&mut self, except: TokenType) -> Result<Token> {
        if let Some(token) = self.lexer.next() {
            if token.token_type != except {
                return Err(Error::UnexpectedToken(token));
            }
            return Ok(token);
        }

        Err(Error::UnexpectedEOF)
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
    In,
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
            TokenType::Keyword(Keyword::In) => Some(InfixOperator::In),
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
            InfixOperator::In => 7,
        }
    }
}

impl InfixOperator {
    pub fn build(&self, lhr: Expression, rhs: Expression) -> Result<Expression> {
        match self {
            InfixOperator::Add => Ok(Expression::Operator(ast::Operator::Add(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Sub => Ok(Expression::Operator(ast::Operator::Sub(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Mul => Ok(Expression::Operator(ast::Operator::Mul(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Div => Ok(Expression::Operator(ast::Operator::Div(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Gt => Ok(Expression::Operator(ast::Operator::Gt(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Gte => Ok(Expression::Operator(ast::Operator::Gte(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Lt => Ok(Expression::Operator(ast::Operator::Lt(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Lte => Ok(Expression::Operator(ast::Operator::Lte(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Eq => Ok(Expression::Operator(ast::Operator::Eq(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::NotEq => Ok(Expression::Operator(ast::Operator::NotEq(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::And => Ok(Expression::Operator(ast::Operator::And(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            InfixOperator::Or => Ok(Expression::Operator(ast::Operator::Or(
                Box::new(lhr),
                Box::new(rhs),
            ))),
            _ => return Err(Error::UnKnownInfixOperator(format!("{:?}", self))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::vec;

    use super::Parser;
    use crate::ast::{self, Expression, Statement};
    use crate::datatype::DataType;
    use crate::error::Result;

    #[test]
    fn test_parser_error() {
        let stmt = parse_stmt("SELEC").err().unwrap();
        assert_eq!(
            stmt.to_string(),
            "Unexpected token: 'SELEC' at line: 1, column: 5"
        );

        let stmt = parse_stmt("SELECT * FROM").err().unwrap();
        assert_eq!(stmt.to_string(), "Unexpected EOF");

        let stmt = parse_stmt("SELECT * FROM users WHERE").err().unwrap();
        assert_eq!(stmt.to_string(), "Unexpected EOF");
    }

    #[test]
    fn test_parse_create_table() -> Result<()> {
        let stmt = parse_stmt("CREATE TABLE t1(i INTEGER, j INTEGER);")?;

        assert_eq!(
            stmt,
            Statement::CreateTable {
                table: "t1".to_owned(),
                columns: vec![
                    ast::Column {
                        name: "i".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false,
                    },
                    ast::Column {
                        name: "j".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false
                    },
                ],
                check_exists: false,
            }
        );

        let stmt = parse_stmt("CREATE TABLE IF NOT EXISTS t1(i INTEGER, j INTEGER);")?;

        assert_eq!(
            stmt,
            Statement::CreateTable {
                table: "t1".to_owned(),
                columns: vec![
                    ast::Column {
                        name: "i".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false,
                    },
                    ast::Column {
                        name: "j".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false
                    },
                ],
                check_exists: true,
            }
        );

        let stmt = parse_stmt("CREATE TABLE t1(i INTEGER PRIMARY KEY, j INTEGER);")?;

        assert_eq!(
            stmt,
            Statement::CreateTable {
                table: "t1".to_owned(),
                columns: vec![
                    ast::Column {
                        name: "i".to_owned(),
                        datatype: DataType::Integer,
                        nullable: false,
                        unique: false,
                        references: None,
                        primary_key: true,
                        index: false,
                    },
                    ast::Column {
                        name: "j".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false
                    },
                ],
                check_exists: false,
            }
        );

        let stmt = parse_stmt("CREATE TABLE t1(i INTEGER UNIQUE, j INTEGER);")?;

        assert_eq!(
            stmt,
            Statement::CreateTable {
                table: "t1".to_owned(),
                columns: vec![
                    ast::Column {
                        name: "i".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: true,
                        references: None,
                        primary_key: false,
                        index: false,
                    },
                    ast::Column {
                        name: "j".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false
                    },
                ],
                check_exists: false,
            }
        );

        let stmt = parse_stmt("CREATE TABLE t1(i INTEGER NOT NULL, j INTEGER);")?;

        assert_eq!(
            stmt,
            Statement::CreateTable {
                table: "t1".to_owned(),
                columns: vec![
                    ast::Column {
                        name: "i".to_owned(),
                        datatype: DataType::Integer,
                        nullable: false,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false,
                    },
                    ast::Column {
                        name: "j".to_owned(),
                        datatype: DataType::Integer,
                        nullable: true,
                        unique: false,
                        references: None,
                        primary_key: false,
                        index: false
                    },
                ],
                check_exists: false,
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_drop_schema() -> Result<()> {
        let stmt = parse_stmt("DROP SCHEMA test;")?;

        assert_eq!(
            stmt,
            Statement::DropSchema {
                schema: "test".to_owned(),
                check_exists: false,
            }
        );

        let stmt = parse_stmt("DROP SCHEMA IF EXISTS test;")?;

        assert_eq!(
            stmt,
            Statement::DropSchema {
                schema: "test".to_owned(),
                check_exists: true,
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_create_schema() -> Result<()> {
        let stmt = parse_stmt("CREATE SCHEMA test;")?;

        assert_eq!(
            stmt,
            Statement::CreateSchema {
                schema: "test".to_owned(),
                check_exists: false,
            }
        );

        let stmt = parse_stmt("CREATE SCHEMA IF NOT EXISTS test;")?;

        assert_eq!(
            stmt,
            Statement::CreateSchema {
                schema: "test".to_owned(),
                check_exists: true,
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_delete_statement() -> Result<()> {
        let stmt = parse_stmt("DELETE FROM users;")?;

        assert_eq!(
            stmt,
            Statement::Delete {
                table: "users".to_owned(),
                r#where: None,
            }
        );

        let stmt = parse_stmt("DELETE FROM users WHERE id = 1;")?;

        assert_eq!(
            stmt,
            Statement::Delete {
                table: "users".to_owned(),
                r#where: Some(Expression::Operator(ast::Operator::Eq(
                    Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                ))),
            }
        );

        Ok(())
    }

    #[test]
    fn test_parse_update_statement() {
        let stmt = parse_stmt("UPDATE users SET name = 'name'").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Update {
                table: "users".to_owned(),
                assignments: BTreeMap::from([(
                    "name".to_owned(),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned()))
                )]),
                r#where: None,
            }
        );

        let stmt = parse_stmt("UPDATE users SET name = 'name' WHERE id = 1").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Update {
                table: "users".to_owned(),
                assignments: BTreeMap::from([(
                    "name".to_owned(),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned()))
                )]),
                r#where: Some(ast::Expression::Operator(ast::Operator::Eq(
                    Box::new(ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))),
                    Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                ))),
            }
        );

        let stmt = parse_stmt("UPDATE users SET name = 'name', id = 1 WHERE id = 1;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Update {
                table: "users".to_owned(),
                assignments: BTreeMap::from([
                    (
                        "name".to_owned(),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned()))
                    ),
                    (
                        "id".to_owned(),
                        ast::Expression::Literal(ast::Literal::Int(1))
                    ),
                ]),
                r#where: Some(ast::Expression::Operator(ast::Operator::Eq(
                    Box::new(ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))),
                    Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                ))),
            }
        );
    }

    #[test]
    fn test_parse_insert_statement() {
        let stmt = parse_stmt("INSERT INTO users VALUES (1, 'name');").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: None,
                values: vec![vec![
                    ast::Expression::Literal(ast::Literal::Int(1)),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]],
                on_conflict: None,
                returning: None,
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name');").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![vec![
                    ast::Expression::Literal(ast::Literal::Int(1)),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]],
                on_conflict: None,
                returning: None,
            }
        );

        let stmt =
            parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2');").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: None,
                returning: None,
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2') ON CONFLICT (id) DO NOTHING;")
            .unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: Some(ast::OnConflict::DoNothing),
                returning: None,
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2') ON CONFLICT (id) DO UPDATE SET name = 'name';")
            .unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: Some(ast::OnConflict::DoUpdate {
                    constraints: vec![ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))],
                    values: vec![ast::Expression::Operator(ast::Operator::Eq(
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "name".to_owned()
                        ))),
                        Box::new(ast::Expression::Literal(ast::Literal::String(
                            "name".to_owned()
                        ))),
                    ))],
                }),
                returning: None,
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2') ON CONFLICT (id) DO UPDATE SET name = 'name', id = 1;")
            .unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: Some(ast::OnConflict::DoUpdate {
                    constraints: vec![ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))],
                    values: vec![
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                        )),
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "id".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                        )),
                    ],
                }),
                returning: None,
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2') ON CONFLICT (id) DO UPDATE SET name = 'name', id = 1 RETURNING id;")
            .unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: Some(ast::OnConflict::DoUpdate {
                    constraints: vec![ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))],
                    values: vec![
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                        )),
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "id".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                        )),
                    ],
                }),
                returning: Some(vec![(
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    None,
                )]),
            }
        );

        let stmt = parse_stmt("INSERT INTO users (id, name) VALUES (1, 'name'), (2, 'name2') ON CONFLICT (id) DO UPDATE SET name = 'name', id = 1 RETURNING id AS user_id;")
            .unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Insert {
                table: (String::from("users"), None,),
                columns: Some(vec![
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
                values: vec![
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(1)),
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ],
                    vec![
                        ast::Expression::Literal(ast::Literal::Int(2)),
                        ast::Expression::Literal(ast::Literal::String("name2".to_owned())),
                    ],
                ],
                on_conflict: Some(ast::OnConflict::DoUpdate {
                    constraints: vec![ast::Expression::Literal(ast::Literal::String(
                        "id".to_owned()
                    ))],
                    values: vec![
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "name".to_owned()
                            ))),
                        )),
                        ast::Expression::Operator(ast::Operator::Eq(
                            Box::new(ast::Expression::Literal(ast::Literal::String(
                                "id".to_owned()
                            ))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                        )),
                    ],
                }),
                returning: Some(vec![(
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    Some(String::from("user_id")),
                )]),
            }
        );
    }

    #[test]
    fn test_parse_select_statement() {
        let stmt = parse_stmt("SELECT * FROM users;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
                having: None,
            }
        );

        let stmt = parse_stmt("SELECT 1").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                columns: vec![(ast::Expression::Literal(ast::Literal::Int(1)), None,)],
                from: None,
                r#where: None,
                group_by: None,
                having: None,
            }
        );
    }

    #[test]
    fn test_parse_from_item() {
        let stmt = parse_stmt("select * from public.users as u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("public.users"),
                    alias: Some(String::from("u")),
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from catalog.public.users u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("catalog.public.users"),
                    alias: Some(String::from("u")),
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from catalog.public.users(1,'2',box(1));").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::TableFunction {
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
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from (select * from users) as u;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::SubQuery {
                    query: Box::new(ast::Statement::Select {
                        order_by: None,
                        limit: None,
                        offset: None,
                        having: None,
                        distinct: None,
                        columns: vec![(
                            ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                            None,
                        )],
                        from: Some(ast::From::Table {
                            name: String::from("users"),
                            alias: None,
                        }),
                        r#where: None,
                        group_by: None,
                    }),
                    alias: Some(String::from("u")),
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Join {
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
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u left join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Join {
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
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt =
            parse_stmt("select * from users u right join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                distinct: None,
                having: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Join {
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
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u full join users u2 on u.id = u2.id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Join {
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
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("select * from users u cross join users u2;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Join {
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
                }),
                r#where: None,
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_order_by() {
        let stmt = parse_stmt("SELECT * FROM users ORDER BY id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: Some(vec![(
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Order::Asc,
                )]),
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users ORDER BY id ASC;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: Some(vec![(
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Order::Asc,
                )]),
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users ORDER BY id,name,age;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: Some(vec![
                    (
                        ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                        ast::Order::Asc,
                    ),
                    (
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                        ast::Order::Asc,
                    ),
                    (
                        ast::Expression::Literal(ast::Literal::String("age".to_owned())),
                        ast::Order::Asc,
                    ),
                ]),
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users ORDER BY id DESC;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: Some(vec![(
                    ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                    ast::Order::Desc,
                )]),
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users ORDER BY id DESC, name ASC;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: Some(vec![
                    (
                        ast::Expression::Literal(ast::Literal::String("id".to_owned())),
                        ast::Order::Desc,
                    ),
                    (
                        ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                        ast::Order::Asc,
                    ),
                ]),
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_limit_offset() {
        let stmt = parse_stmt("SELECT * FROM users LIMIT 10;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: Some(ast::Expression::Literal(ast::Literal::Int(10))),
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users LIMIT 10 OFFSET 10;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: Some(ast::Expression::Literal(ast::Literal::Int(10))),
                offset: Some(ast::Expression::Literal(ast::Literal::Int(10))),
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
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
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: Some(ast::Distinct::ALL),
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT DISTINCT ON(name,age),school FROM users;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: Some(ast::Distinct::DISTINCT(vec![
                    ast::Expression::Literal(ast::Literal::String("name".to_owned())),
                    ast::Expression::Literal(ast::Literal::String("age".to_owned())),
                ])),
                columns: vec![(
                    ast::Expression::Literal(ast::Literal::String("school".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: None,
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_where() {
        let stmt = parse_stmt("SELECT * FROM users WHERE id = 1;").unwrap();
        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::Operator(ast::Operator::Eq(
                    Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                ))),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id = 1 AND name = 'foo';").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::Operator(ast::Operator::And(
                    Box::new(Expression::Operator(ast::Operator::Eq(
                        Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                        Box::new(Expression::Literal(ast::Literal::Int(1))),
                    ))),
                    Box::new(Expression::Operator(ast::Operator::Eq(
                        Box::new(Expression::Literal(ast::Literal::String("name".to_owned()))),
                        Box::new(Expression::Literal(ast::Literal::String("foo".to_owned()))),
                    ))),
                ))),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id = 1 OR name = 'foo';").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::Operator(ast::Operator::Or(
                    Box::new(Expression::Operator(ast::Operator::Eq(
                        Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                        Box::new(Expression::Literal(ast::Literal::Int(1))),
                    ))),
                    Box::new(Expression::Operator(ast::Operator::Eq(
                        Box::new(Expression::Literal(ast::Literal::String("name".to_owned()))),
                        Box::new(Expression::Literal(ast::Literal::String("foo".to_owned()))),
                    ))),
                ))),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id in (1,2,3)").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::InList {
                    field: Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    list: vec![
                        Expression::Literal(ast::Literal::Int(1)),
                        Expression::Literal(ast::Literal::Int(2)),
                        Expression::Literal(ast::Literal::Int(3)),
                    ],
                    negated: false,
                }),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id not in (1,2,3)").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::InList {
                    field: Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    list: vec![
                        Expression::Literal(ast::Literal::Int(1)),
                        Expression::Literal(ast::Literal::Int(2)),
                        Expression::Literal(ast::Literal::Int(3)),
                    ],
                    negated: true,
                }),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id in ('1','2')").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::InList {
                    field: Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    list: vec![
                        Expression::Literal(ast::Literal::String("1".to_owned())),
                        Expression::Literal(ast::Literal::String("2".to_owned())),
                    ],
                    negated: false,
                }),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id not in ('1','2')").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::InList {
                    field: Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    list: vec![
                        Expression::Literal(ast::Literal::String("1".to_owned())),
                        Expression::Literal(ast::Literal::String("2".to_owned())),
                    ],
                    negated: true,
                }),
                group_by: None,
            }
        );

        let stmt = parse_stmt("SELECT * FROM users WHERE id in (select id from users)").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: String::from("users"),
                    alias: None,
                }),
                r#where: Some(Expression::InSubQuery {
                    field: Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    query: Box::new(ast::Statement::Select {
                        order_by: None,
                        limit: None,
                        offset: None,
                        having: None,
                        distinct: None,
                        columns: vec![(
                            Expression::Literal(ast::Literal::String("id".to_owned())),
                            None,
                        )],
                        from: Some(ast::From::Table {
                            name: String::from("users"),
                            alias: None,
                        }),
                        r#where: None,
                        group_by: None,
                    }),
                    negated: false,
                }),
                group_by: None,
            }
        );
    }

    #[test]
    fn test_parse_group_by() {
        let stmt = parse_stmt("SELECT * FROM users GROUP BY id;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: "users".to_owned(),
                    alias: None,
                }),
                r#where: None,
                group_by: Some(vec![Expression::Literal(ast::Literal::String(
                    "id".to_owned()
                ))]),
            }
        );

        let stmt = parse_stmt("SELECT * FROM users GROUP BY id, name;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: None,
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: "users".to_owned(),
                    alias: None,
                }),
                r#where: None,
                group_by: Some(vec![
                    Expression::Literal(ast::Literal::String("id".to_owned())),
                    Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
            }
        );

        let stmt = parse_stmt("SELECT * FROM users GROUP BY id, name HAVING id = 1;").unwrap();

        assert_eq!(
            stmt,
            ast::Statement::Select {
                order_by: None,
                limit: None,
                offset: None,
                having: Some(Expression::Operator(ast::Operator::Eq(
                    Box::new(Expression::Literal(ast::Literal::String("id".to_owned()))),
                    Box::new(Expression::Literal(ast::Literal::Int(1))),
                ))),
                distinct: None,
                columns: vec![(
                    Expression::Literal(ast::Literal::String("*".to_owned())),
                    None,
                )],
                from: Some(ast::From::Table {
                    name: "users".to_owned(),
                    alias: None,
                }),
                r#where: None,
                group_by: Some(vec![
                    Expression::Literal(ast::Literal::String("id".to_owned())),
                    Expression::Literal(ast::Literal::String("name".to_owned())),
                ]),
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
        let mut parser = Parser::new(input);
        parser.parse()
    }

    fn parse_expr(input: &str) -> Result<Expression> {
        let mut parser = Parser::new(input);
        parser.parse_expression(0)
    }
}
