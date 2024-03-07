use std::{
    collections::BTreeMap,
    fmt::{Display, Formatter},
};

use crate::{datatype::DataType, error::Error};

#[derive(PartialEq, Debug)]
pub enum Statement {
    CreateTable {
        table: String,
        check_exists: bool,
        columns: Vec<Column>,
    },
    CreateSchema {
        schema: String,
        check_exists: bool,
    },
    DropTable {
        table: String,
        check_exists: bool,
    },
    DropSchema {
        schema: String,
        check_exists: bool,
    },
    Select {
        distinct: Option<Distinct>,
        columns: Vec<SelectItem>,
        from: Option<From>,
        r#where: Option<Expression>,
        group_by: Option<Vec<Expression>>,
        having: Option<Expression>,
        order_by: Option<Vec<(Expression, Order)>>,
        limit: Option<Expression>,
        offset: Option<Expression>,
    },
    Insert {
        table: (String, Option<String>),
        columns: Option<Vec<Expression>>,
        values: Vec<Vec<Expression>>,
        on_conflict: Option<OnConflict>,
        returning: Option<Vec<SelectItem>>,
    },
    Update {
        table: String,
        assignments: BTreeMap<String, Expression>,
        r#where: Option<Expression>,
    },
    Delete {
        table: String,
        r#where: Option<Expression>,
    },
}

#[derive(PartialEq, Debug)]
pub enum SelectItem {
    /// expression without alias eg. `SELECT 1`
    UnNamedExpr(Expression),
    /// expression with alias eg. `SELECT 1 AS one`
    ExprWithAlias(Expression, String),
    /// `SELECT *`
    Wildcard,
    /// `SELECT table.*`
    QualifiedWildcard(String),
}

impl Display for SelectItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SelectItem::UnNamedExpr(e) => write!(f, "{}", e),
            SelectItem::ExprWithAlias(e, a) => write!(f, "{} AS {}", e, a),
            SelectItem::Wildcard => write!(f, "*"),
            SelectItem::QualifiedWildcard(t) => write!(f, "{}.*", t),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Column {
    pub name: String,
    pub datatype: DataType,
    pub primary_key: bool,
    pub nullable: bool,
    pub index: bool,
    pub unique: bool,
    pub references: Option<String>,
}

impl Display for Column {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.datatype)?;
        if self.primary_key {
            write!(f, " PRIMARY KEY")?;
        }
        if self.nullable {
            write!(f, " NULL")?;
        } else {
            write!(f, " NOT NULL")?;
        }
        if self.index {
            write!(f, " INDEX")?;
        }
        if self.unique {
            write!(f, " UNIQUE")?;
        }
        if let Some(r) = &self.references {
            write!(f, " REFERENCES {}", r)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug)]
pub enum OnConflict {
    DoNothing,
    /// `ON CONFLICT (constraint) DO UPDATE SET values[expressions]`
    DoUpdate {
        constraints: Vec<Ident>,
        values: Vec<Expression>,
    },
}

#[derive(PartialEq, Debug)]
pub struct Ident {
    pub value: String,
    pub quote_style: Option<char>,
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.quote_style {
            Some(q) => write!(f, "{}{}{}", q, self.value, q),
            None => write!(f, "{}", self.value),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Order {
    Asc,
    Desc,
}

impl Display for OnConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OnConflict::DoNothing => write!(f, "DO NOTHING"),
            OnConflict::DoUpdate {
                constraints,
                values,
            } => {
                write!(
                    f,
                    "DO UPDATE SET {}",
                    values
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
                if !constraints.is_empty() {
                    write!(
                        f,
                        " WHERE {}",
                        constraints
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Order {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Order::Asc => write!(f, "ASC"),
            Order::Desc => write!(f, "DESC"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Select {
                distinct,
                columns,
                from,
                r#where,
                group_by,
                having,
                order_by,
                limit,
                offset,
            } => {
                write!(f, "SELECT ")?;
                if let Some(d) = distinct {
                    match d {
                        Distinct::ALL => write!(f, "ALL ")?,
                        Distinct::DISTINCT(e) => {
                            write!(f, "DISTINCT ")?;
                            write!(
                                f,
                                "{}",
                                e.iter()
                                    .map(|e| e.to_string())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            )?;
                        }
                    }
                }
                write!(
                    f,
                    "{}",
                    columns
                        .iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
                if let Some(from) = from {
                    write!(f, " FROM {}", from)?;
                }
                if let Some(where_) = r#where {
                    write!(f, " WHERE {}", where_)?;
                }
                if let Some(group_by) = group_by {
                    write!(
                        f,
                        " GROUP BY {}",
                        group_by
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }

                if let Some(having) = having {
                    write!(f, " HAVING {}", having)?;
                }

                if let Some(order_by) = order_by {
                    write!(
                        f,
                        " ORDER BY {}",
                        order_by
                            .iter()
                            .map(|(e, o)| format!("{} {}", e, o))
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }

                if let Some(limit) = limit {
                    write!(f, " LIMIT {}", limit)?;
                }

                if let Some(offset) = offset {
                    write!(f, " OFFSET {}", offset)?;
                }
                Ok(())
            }
            Statement::Insert {
                table,
                columns,
                values,
                on_conflict,
                returning,
            } => {
                write!(f, "INSERT INTO {} ", table.0)?;
                if let Some(a) = &table.1 {
                    write!(f, "AS {} ", a)?;
                }
                if let Some(c) = columns {
                    write!(
                        f,
                        "({}) ",
                        c.iter()
                            .map(|c| c.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }
                write!(
                    f,
                    "VALUES {}",
                    values
                        .iter()
                        .map(|v| {
                            format!(
                                "({})",
                                v.iter()
                                    .map(|e| e.to_string())
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            )
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;

                if let Some(o) = on_conflict {
                    write!(f, " ON CONFLICT {}", o)?;
                }

                if let Some(r) = returning {
                    write!(
                        f,
                        " RETURNING {}",
                        r.iter()
                            .map(|item| item.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
            Statement::Update {
                table,
                assignments,
                r#where,
            } => {
                write!(
                    f,
                    "UPDATE {} SET {}",
                    table,
                    assignments
                        .into_iter()
                        .map(|(k, v)| format!("{} = {}", k, v))
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;

                if let Some(w) = r#where {
                    write!(f, "WHERE {}", w)?;
                }

                Ok(())
            }
            Statement::Delete { table, r#where } => {
                write!(f, "DELETE FROM {}", table)?;
                if let Some(w) = r#where {
                    write!(f, " WHERE {}", w)?;
                }
                Ok(())
            }
            Statement::CreateSchema {
                schema,
                check_exists,
            } => {
                write!(f, "CREATE SCHEMA ")?;
                if *check_exists {
                    write!(f, "IF NOT EXISTS ")?;
                }
                write!(f, "{}", schema)
            }
            Statement::DropSchema {
                schema,
                check_exists,
            } => {
                write!(f, "DROP SCHEMA ")?;
                if *check_exists {
                    write!(f, "IF EXISTS ")?;
                }
                write!(f, "{}", schema)
            }
            Statement::CreateTable {
                table,
                check_exists,
                columns,
            } => {
                write!(f, "CREATE TABLE ")?;
                if *check_exists {
                    write!(f, "IF NOT EXISTS ")?;
                }
                write!(f, "{} (", table)?;
                for (i, c) in columns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", c)?;
                }
                write!(f, ")")
            }
            Statement::DropTable {
                table,
                check_exists,
            } => {
                write!(f, "DROP TABLE ")?;
                if *check_exists {
                    write!(f, "IF EXISTS ")?;
                }
                write!(f, "{}", table)
            }
        }
    }
}

#[derive(PartialEq, Debug)]

pub enum Distinct {
    ALL,
    DISTINCT(Vec<Expression>),
}

#[derive(PartialEq, Debug)]

pub enum From {
    Table {
        name: String,
        alias: Option<String>,
    },
    TableFunction {
        name: String,
        args: Vec<Assignment>,
        alias: Option<String>,
    },
    SubQuery {
        query: Box<Statement>,
        alias: Option<String>,
    },
    Join {
        left: Box<From>,
        right: Box<From>,
        on: Option<Expression>,
        join_type: JoinType,
    },
}

impl Display for From {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            From::Table { name, alias } => match alias {
                Some(a) => write!(f, "{} AS {}", name, a),
                None => write!(f, "{}", name),
            },
            From::TableFunction { name, args, alias } => match alias {
                Some(a) => write!(
                    f,
                    "{}({}) AS {}",
                    name,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    a
                ),
                None => write!(
                    f,
                    "{}({})",
                    name,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
            },
            From::SubQuery { query, alias } => match alias {
                Some(a) => write!(f, "({}) AS {}", query, a),
                None => write!(f, "({})", query),
            },
            From::Join {
                left,
                right,
                on,
                join_type,
            } => {
                write!(f, "{}", left)?;
                match join_type {
                    JoinType::Cross => write!(f, " CROSS JOIN ")?,
                    JoinType::Inner => write!(f, " INNER JOIN ")?,
                    JoinType::Left => write!(f, " LEFT JOIN ")?,
                    JoinType::Full => write!(f, " FULL JOIN ")?,
                    JoinType::Right => write!(f, " RIGHT JOIN ")?,
                }
                write!(f, "{}", right)?;
                if let Some(on) = on {
                    write!(f, " ON {}", on)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum JoinType {
    Cross,
    Inner,
    Left,
    Full,
    Right,
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(String),
    Literal(Literal),
    BinaryOperator(BinaryOperator),
    Function(String, Vec<Expression>),
    Struct(Vec<StructField>),
    Array(Vec<Expression>),
    /// `[ NOT ] IN (val1, val2, ...)`
    InList {
        field: Box<Expression>,
        list: Vec<Expression>,
        negated: bool,
    },
    /// `[ NOT ] IN (SELECT ...)`
    InSubQuery {
        field: Box<Expression>,
        query: Box<Statement>,
        negated: bool,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::BinaryOperator(o) => write!(f, "{}", o),
            Expression::Function(n, args) => {
                write!(
                    f,
                    "{}({})",
                    n,
                    args.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expression::InList {
                field,
                list,
                negated,
            } => {
                write!(
                    f,
                    "{} {} IN ({})",
                    field,
                    if *negated { "NOT" } else { "" },
                    list.iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expression::InSubQuery {
                field,
                query,
                negated,
            } => {
                write!(
                    f,
                    "{} {} IN ({})",
                    field,
                    if *negated { "NOT" } else { "" },
                    query
                )
            }
            Expression::Identifier(i) => write!(f, "{}", i),
            Expression::Struct(s) => write!(
                f,
                "({})",
                s.iter()
                    .map(|field| format!("{} = {}", field.name, field.value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct StructField {
    pub name: Expression,
    pub value: Expression,
}

#[derive(PartialEq, Debug)]
pub struct Assignment {
    pub id: Option<Ident>,
    pub value: Expression,
}

impl Display for Assignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.id {
            Some(id) => write!(f, "{} = {}", id, self.value),
            None => write!(f, "{}", self.value),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Null => write!(f, "null"),
        }
    }
}

impl TryInto<i64> for Literal {
    type Error = Error;

    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Literal::Int(i) => Ok(i),
            _ => Err(Error::ParserError(format!(
                "{} cannot be converted to i64",
                self.to_string()
            ))),
        }
    }
}

impl TryInto<f64> for Literal {
    type Error = Error;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Literal::Float(f) => Ok(f),
            _ => Err(Error::ParserError(format!(
                "{} cannot be converted to f64",
                self.to_string()
            ))),
        }
    }
}

impl TryInto<String> for Literal {
    type Error = Error;

    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Literal::String(s) => Ok(s),
            _ => Err(Error::ParserError(format!(
                "{} cannot be converted to String",
                self.to_string()
            ))),
        }
    }
}

impl TryInto<bool> for Literal {
    type Error = Error;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Literal::Boolean(b) => Ok(b),
            _ => Err(Error::ParserError(format!(
                "{} cannot be converted to bool",
                self.to_string()
            ))),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    // Logical
    Not(Box<Expression>),
    Eq(Box<Expression>, Box<Expression>),
    NotEq(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Gte(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Lte(Box<Expression>, Box<Expression>),

    // Mathematical
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Neg(Box<Expression>),
    Pos(Box<Expression>),
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Not(e) => write!(f, "NOT {}", e),
            BinaryOperator::Sub(l, r) => write!(f, "{} - {}", l, r),
            BinaryOperator::Neg(e) => write!(f, "-{}", e),
            BinaryOperator::Pos(r) => write!(f, "+{}", r),
            BinaryOperator::Add(l, r) => write!(f, "{} + {}", l, r),
            BinaryOperator::Mul(l, r) => write!(f, "{} * {}", l, r),
            BinaryOperator::Div(l, r) => write!(f, "{} / {}", l, r),
            BinaryOperator::Eq(l, r) => write!(f, "{} = {}", l, r),
            BinaryOperator::NotEq(l, r) => write!(f, "{} != {}", l, r),
            BinaryOperator::And(l, r) => write!(f, "{} AND {}", l, r),
            BinaryOperator::Or(l, r) => write!(f, "{} OR {}", l, r),
            BinaryOperator::Gt(l, r) => write!(f, "{} > {}", l, r),
            BinaryOperator::Gte(l, r) => write!(f, "{} >= {}", l, r),
            BinaryOperator::Lt(l, r) => write!(f, "{} < {}", l, r),
            BinaryOperator::Lte(l, r) => write!(f, "{} <= {}", l, r),
        }
    }
}
