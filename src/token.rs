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
    In,
    On,
    As,
    True,
    False,
    Join,
    Inner,
    Left,
    Right,
    Full,
    Cross,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    ILLIGAL,
    EOF,
    // Identifiers + literals
    Ident,
    String,
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
    Period,

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
            "on" => TokenType::Keyword(Keyword::On),
            "in" => TokenType::Keyword(Keyword::In),
            "distinct" => TokenType::Keyword(Keyword::Distinct),
            "true" => TokenType::Keyword(Keyword::True),
            "false" => TokenType::Keyword(Keyword::False),
            "join" => TokenType::Keyword(Keyword::Join),
            "inner" => TokenType::Keyword(Keyword::Inner),
            "left" => TokenType::Keyword(Keyword::Left),
            "right" => TokenType::Keyword(Keyword::Right),
            "full" => TokenType::Keyword(Keyword::Full),
            "cross" => TokenType::Keyword(Keyword::Cross),
            "(" => TokenType::LParen,
            ")" => TokenType::RParen,
            "{" => TokenType::LBrace,
            "}" => TokenType::RBrace,
            "," => TokenType::Comma,
            ";" => TokenType::Semicolon,
            "+" => TokenType::Plus,
            "-" => TokenType::Minus,
            "*" => TokenType::Asterisk,
            "/" => TokenType::Slash,
            "<" => TokenType::Lt,
            ">" => TokenType::Gt,
            "=" => TokenType::Eq,
            "!" => TokenType::Bang,
            "." => TokenType::Period,
            "<=" => TokenType::Lte,
            ">=" => TokenType::Gte,
            "!=" => TokenType::NotEq,
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
