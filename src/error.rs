use crate::token::Token;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF,
    DuplicateColumn(String),
    UnKnownInfixOperator(String),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    UnKnownDataType(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnexpectedEOF => write!(f, "Unexpected EOF"),
            Error::UnKnownInfixOperator(op) => write!(f, "Unknown infix operator: {}", op),
            Error::UnexpectedToken(tok) => {
                write!(
                    f,
                    "Unexpected token: '{}' at line: {}, column: {}",
                    tok.literal, tok.line, tok.column
                )
            }
            Error::ParseIntError(e) => write!(f, "Parse int error: {}", e),
            Error::DuplicateColumn(col) => write!(f, "Duplicate values given for column {}", col),
            Error::UnKnownDataType(dtype) => write!(f, "Unknown datatype: {}", dtype),
        }
    }
}
