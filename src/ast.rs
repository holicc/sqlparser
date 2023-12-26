use std::fmt::{Display, Formatter};

#[derive(PartialEq, Debug)]
pub enum Statement {
    Select {
        distinct: Option<Distinct>,
        columns: Vec<(Expression, Option<String>)>,
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
        returning: Option<Vec<(Expression, Option<String>)>>,
    },
}

#[derive(PartialEq, Debug)]
pub enum OnConflict {
    DoNothing,
    /// `ON CONFLICT (constraint) DO UPDATE SET values[expressions]`
    DoUpdate {
        constraints: Vec<Expression>,
        values: Vec<Expression>,
    },
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
                        .map(|(e, a)| match a {
                            Some(a) => format!("{} AS {}", e, a),
                            None => e.to_string(),
                        })
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
                            .map(|(e, a)| match a {
                                Some(a) => format!("{} AS {}", e, a),
                                None => e.to_string(),
                            })
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }
                Ok(())
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
        args: Vec<Expression>,
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
    Literal(Literal),
    Operator(Operator),
    Function(String, Vec<Expression>),
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
            Expression::Operator(o) => write!(f, "{}", o),
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

#[derive(PartialEq, Debug)]
pub enum Operator {
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

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Not(e) => write!(f, "NOT {}", e),
            Operator::Sub(l, r) => write!(f, "{} - {}", l, r),
            Operator::Neg(e) => write!(f, "-{}", e),
            Operator::Pos(r) => write!(f, "+{}", r),
            Operator::Add(l, r) => write!(f, "{} + {}", l, r),
            Operator::Mul(l, r) => write!(f, "{} * {}", l, r),
            Operator::Div(l, r) => write!(f, "{} / {}", l, r),
            Operator::Eq(l, r) => write!(f, "{} = {}", l, r),
            Operator::NotEq(l, r) => write!(f, "{} != {}", l, r),
            Operator::And(l, r) => write!(f, "{} AND {}", l, r),
            Operator::Or(l, r) => write!(f, "{} OR {}", l, r),
            Operator::Gt(l, r) => write!(f, "{} > {}", l, r),
            Operator::Gte(l, r) => write!(f, "{} >= {}", l, r),
            Operator::Lt(l, r) => write!(f, "{} < {}", l, r),
            Operator::Lte(l, r) => write!(f, "{} <= {}", l, r),
        }
    }
}
