use std::fmt::Display;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Select {
        distinct: Option<Distinct>,
        columns: Vec<(Expression, Option<String>)>,
        from: From,
        r#where: Option<Expression>,
        group_by: Option<Vec<Expression>>,
    },
}

#[derive(PartialEq, Debug)]

pub enum Distinct {
    ALL,
    DISTINCT(Vec<Expression>),
}

#[derive(PartialEq, Eq, Debug)]

pub enum From {
    Table { name: String, alias: Option<String> },
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
    Operator(Operator),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Operator(o) => write!(f, "{}", o),
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
