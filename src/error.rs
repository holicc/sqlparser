use crate::token::Token;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF,
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
}
