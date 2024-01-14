use crate::token::Token;
use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF(String, Token),
    DuplicateColumn(String, Token),
    UnKnownInfixOperator(String, Token),
    UnexpectedToken(String, Token),
    ParseIntError(String, Token, std::num::ParseIntError),
    UnKnownDataType(String, Token),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnexpectedEOF(line, token) => {
                self.diagnosis(f, "unexpected EOF", token.line, token.column, &line)
            }
            Error::DuplicateColumn(line, token) => {
                self.diagnosis(f, "duplicate column", token.line, token.column, &line)
            }
            Error::UnKnownInfixOperator(line, token) => {
                self.diagnosis(f, "unknown infix operator", token.line, token.column, &line)
            }
            Error::UnexpectedToken(line, token) => {
                self.diagnosis(f, "unexpected token", token.line, token.column, &line)
            }
            Error::ParseIntError(line, token, err) => self.diagnosis(
                f,
                &format!("parse int error: {}", err),
                token.line,
                token.column,
                &line,
            ),
            Error::UnKnownDataType(line, token) => {
                self.diagnosis(f, "unknown data type", token.line, token.column, &line)
            }
        }
    }
}

impl Error {
    fn diagnosis(
        &self,
        f: &mut std::fmt::Formatter,
        reason: &str,
        line: usize,
        column: usize,
        line_str: &str,
    ) -> std::fmt::Result {
        write!(
            f,
            "\x1b[1;34m--->\x1b[0m error: {}: {}:{} \n",
            reason, line, column
        )?;
        writeln!(f, "\x1b[1;34m {} |\x1b[0m ", line)?;
        writeln!(f, "\x1b[1;34m {} |        \x1b[0m {}", line, line_str)?;
        let empty_spaces = " ".repeat(column - 1);
        // using '^' to location of token position
        write!(
            f,
            "\x1b[1;34m {} |        \x1b[0m {}\x1b[1;31m^ no help message\x1b[0m",
            line, empty_spaces
        )
    }
}

fn red(s: &str) -> String {
    format!("\x1b[1;31m{}\x1b[0m", s)
}

fn blue(s: &str) -> String {
    format!("\x1b[1;34m{}\x1b[0m", s)
}
