use crate::token::Token;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF,
    UnKnownInfixOperator(String),
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
}
