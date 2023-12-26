use std::{iter::Peekable, str::Chars};

use crate::token::{Token, TokenType};

const EMPTY_CHAR: char = '\0';

pub struct Lexer<'a> {
    peekable: Peekable<Chars<'a>>,
    line: usize,
    position: usize,
    ch: char,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.token_type == TokenType::EOF {
            None
        } else {
            Some(tok)
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut peekable = input.chars().peekable();
        Lexer {
            ch: peekable.next().unwrap_or(EMPTY_CHAR),
            peekable,
            line: 1,
            position: 1,
        }
    }

    fn read_char(&mut self) {
        if let Some(ch) = self.peekable.next() {
            self.ch = ch;
        } else {
            self.ch = EMPTY_CHAR;
        }

        if self.ch == '\n' {
            self.line += 1;
        }

        self.position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let literal = char::from(self.ch).to_string();
        let tok = match self.ch {
            EMPTY_CHAR => Token::new(TokenType::EOF, "".to_owned()),
            '=' => Token::new(TokenType::Eq, "=".to_owned()),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_owned())
                } else {
                    Token::new(TokenType::Bang, literal)
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Lte, "<=".to_owned())
                } else {
                    Token::new(TokenType::Lt, literal)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Gte, ">=".to_owned())
                } else {
                    Token::new(TokenType::Gt, literal)
                }
            }
            ';' => Token::new(TokenType::Semicolon, literal),
            '.' => Token::new(TokenType::Period, literal),
            '(' => Token::new(TokenType::LParen, literal),
            ')' => Token::new(TokenType::RParen, literal),
            ',' => Token::new(TokenType::Comma, literal),
            '+' => Token::new(TokenType::Plus, literal),
            '{' => Token::new(TokenType::LBrace, literal),
            '}' => Token::new(TokenType::RBrace, literal),
            '-' => Token::new(TokenType::Minus, literal),
            '*' => Token::new(TokenType::Asterisk, literal),
            '/' => Token::new(TokenType::Slash, literal),
            '?' => Token::new(TokenType::Ident, literal),
            '\'' => {
                let mut s = String::new();
                loop {
                    self.read_char();
                    match self.ch {
                        '\'' => {
                            break;
                        }
                        EMPTY_CHAR => return Token::new(TokenType::ILLIGAL, literal),
                        _ => {
                            s.push(char::from(self.ch));
                        }
                    }
                }
                Token::new(TokenType::String, s)
            }
            b if b.is_ascii_alphabetic() => {
                let literal = self.read_literal();
                let token_type = TokenType::lookup_ident(&literal);
                return Token::new(token_type, literal);
            }
            b if b.is_ascii_digit() => {
                return Token::new(TokenType::Int, self.read_number());
            }
            _ => Token::new(TokenType::ILLIGAL, literal),
        };
        self.read_char();
        tok
    }

    fn read_literal(&mut self) -> String {
        let mut literal = String::new();
        while self.ch.is_ascii_alphabetic() || self.ch.is_ascii_alphanumeric() || self.ch == '_' {
            literal.push(self.ch);
            self.read_char();
        }

        literal
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();
        while self.ch.is_ascii_digit() {
            number.push(self.ch);
            self.read_char();
        }
        number
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&mut self) -> char {
        self.peekable.peek().copied().unwrap_or(EMPTY_CHAR)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Keyword, TokenType};

    #[test]
    fn test_single_char_token() {
        let input = "=-+(){},;*/<>!?";
        let tests = vec![
            (TokenType::Eq, "="),
            (TokenType::Minus, "-"),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::RBrace, "}"),
            (TokenType::Comma, ","),
            (TokenType::Semicolon, ";"),
            (TokenType::Asterisk, "*"),
            (TokenType::Slash, "/"),
            (TokenType::Lt, "<"),
            (TokenType::Gt, ">"),
            (TokenType::Bang, "!"),
        ];
        let mut l = Lexer::new(input);
        for (expected_type, expected_literal) in tests {
            let tok = l.next_token();
            assert_eq!(tok.token_type, expected_type);
            assert_eq!(tok.literal, expected_literal);
        }
    }

    #[test]
    fn test_two_char_token() {
        let input = "=!=<=>=";
        let tests = vec![
            (TokenType::Eq, "="),
            (TokenType::NotEq, "!="),
            (TokenType::Lte, "<="),
            (TokenType::Gte, ">="),
            (TokenType::EOF, ""),
        ];
        let mut l = Lexer::new(input);
        for (expected_type, expected_literal) in tests {
            let tok = l.next_token();
            assert_eq!(tok.token_type, expected_type);
            assert_eq!(tok.literal, expected_literal);
        }
    }

    #[test]
    fn test_next_token() {
        let input = "select distinct * from users as u2 where id = ? and name = ? or age = 12 group by name limit 10;";
        let tests = vec![
            (TokenType::Keyword(Keyword::Select), "select"),
            (TokenType::Keyword(Keyword::Distinct), "distinct"),
            (TokenType::Asterisk, "*"),
            (TokenType::Keyword(Keyword::From), "from"),
            (TokenType::Ident, "users"),
            (TokenType::Keyword(Keyword::As), "as"),
            (TokenType::Ident, "u2"),
            (TokenType::Keyword(Keyword::Where), "where"),
            (TokenType::Ident, "id"),
            (TokenType::Eq, "="),
            (TokenType::Ident, "?"),
            (TokenType::Keyword(Keyword::And), "and"),
            (TokenType::Ident, "name"),
            (TokenType::Eq, "="),
            (TokenType::Ident, "?"),
            (TokenType::Keyword(Keyword::Or), "or"),
            (TokenType::Ident, "age"),
            (TokenType::Eq, "="),
            (TokenType::Int, "12"),
            (TokenType::Keyword(Keyword::Group), "group"),
            (TokenType::Keyword(Keyword::By), "by"),
            (TokenType::Ident, "name"),
            (TokenType::Keyword(Keyword::Limit), "limit"),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
        ];
        let mut l = Lexer::new(input);
        for (expected_type, expected_literal) in tests {
            let tok = l.next_token();
            assert_eq!(tok.token_type, expected_type);
            assert_eq!(tok.literal, expected_literal);
        }
    }
}
