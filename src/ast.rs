use std::fmt::Display;

#[derive(PartialEq, Debug)]
pub enum Statement {
    Select {
        distinct: Option<Distinct>,
        columns: Vec<(Expression, Option<String>)>,
        from: From,
        r#where: Option<Where>,
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

#[derive(PartialEq, Eq, Debug)]

pub enum Where {}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
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