use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    // TODO support full unicode
    ch: u8,
}

impl Iterator for Lexer {
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

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let literal = char::from(self.ch).to_string();
        let tok = match self.ch {
            0 => Token::new(TokenType::EOF, "".to_owned()),
            b'=' => Token::new(TokenType::Eq, "=".to_owned()),
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_owned())
                } else {
                    Token::new(TokenType::Bang, literal)
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::new(TokenType::Lte, "<=".to_owned())
                } else {
                    Token::new(TokenType::Lt, literal)
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::new(TokenType::Gte, ">=".to_owned())
                } else {
                    Token::new(TokenType::Gt, literal)
                }
            }
            b';' => Token::new(TokenType::Semicolon, literal),
            b'.' => Token::new(TokenType::Period, literal),
            b'(' => Token::new(TokenType::LParen, literal),
            b')' => Token::new(TokenType::RParen, literal),
            b',' => Token::new(TokenType::Comma, literal),
            b'+' => Token::new(TokenType::Plus, literal),
            b'{' => Token::new(TokenType::LBrace, literal),
            b'}' => Token::new(TokenType::RBrace, literal),
            b'-' => Token::new(TokenType::Minus, literal),
            b'*' => Token::new(TokenType::Asterisk, literal),
            b'/' => Token::new(TokenType::Slash, literal),
            b'?' => Token::new(TokenType::Ident, literal),
            b'\'' => {
                let mut s = String::new();
                loop {
                    self.read_char();
                    match self.ch {
                        b'\'' => {
                            self.read_char();
                            break;
                        }
                        0 => return Token::new(TokenType::ILLIGAL, literal),
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
        let position = self.position;
        while self.ch.is_ascii_alphabetic() || self.ch.is_ascii_alphanumeric() {
            self.read_char();
        }

        self.input[position..self.position].to_owned()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position].to_owned()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Keyword, TokenType};

    #[test]
    fn test_single_char_token() {
        let input = String::from("=-+(){},;*/<>!?");
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
        let input = String::from("=!=<=>=");
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
        let input = String::from(
            "select distinct * from users as u2 where id = ? and name = ? or age = 12 group by name limit 10;",
        );
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
