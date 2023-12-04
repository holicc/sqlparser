#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Select,
    From,
    Where,
    And,
    Or,
    Not,
    Order,
    By,
    Asc,
    Desc,
    Limit,
    Offset,
    Group,
    Distinct,
    As,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    ILLIGAL,
    EOF,
    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    Lte,
    Gte,

    // Delimiters
    Comma,
    Semicolon,
    Bang,

    Keyword(Keyword),
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident.to_lowercase().as_str() {
            "select" => TokenType::Keyword(Keyword::Select),
            "from" => TokenType::Keyword(Keyword::From),
            "as" => TokenType::Keyword(Keyword::As),
            "where" => TokenType::Keyword(Keyword::Where),
            "and" => TokenType::Keyword(Keyword::And),
            "or" => TokenType::Keyword(Keyword::Or),
            "not" => TokenType::Keyword(Keyword::Not),
            "order" => TokenType::Keyword(Keyword::Order),
            "by" => TokenType::Keyword(Keyword::By),
            "asc" => TokenType::Keyword(Keyword::Asc),
            "desc" => TokenType::Keyword(Keyword::Desc),
            "limit" => TokenType::Keyword(Keyword::Limit),
            "offset" => TokenType::Keyword(Keyword::Offset),
            "group" => TokenType::Keyword(Keyword::Group),
            "distinct" => TokenType::Keyword(Keyword::Distinct),
            _ => TokenType::Ident,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    // FIXME using byte for performance
    pub literal: String,
    // TODO add line and column
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}
