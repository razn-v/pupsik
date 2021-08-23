use std::convert::TryFrom;

/// Tokens are high-level identifiers used by the parser to generate ASTs
#[derive(Debug)]
pub enum Token {
    /// Variables, types and other
    Identifier(String),
    /// Reserved keywords such as types and instructions (return, break etc)
    Reserved(ReservedKind),
    /// Semicolon, parenthesis, curly braces etc
    Separator(SeparatorKind),
    /// Mathematical, bitwise and logical operators. This also includes
    /// assignment operator.
    Operator(OperatorKind),
    /// Strings, integers, floats etc
    Literal(LiteralKind),
}

/// A list specifying categories of reserved keyword
#[derive(Debug)]
pub enum ReservedKind {
    FunctionDecl,
}

impl TryFrom<String> for ReservedKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            "fnc" => Ok(Self::FunctionDecl),
            _     => Err(()),
        };
    }
}

/// A list specifying categories of separator
#[derive(Debug)]
pub enum SeparatorKind {
    Colon,
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Arrow,
}

impl TryFrom<String> for SeparatorKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            ":"  => Ok(Self::Colon),
            ";"  => Ok(Self::Semicolon),
            "("  => Ok(Self::OpenParen),
            ")"  => Ok(Self::CloseParen),
            "{"  => Ok(Self::OpenBrace),
            "}"  => Ok(Self::CloseBrace),
            "->" => Ok(Self::Arrow),
            _    => Err(()),
        };
    }
}

/// A list specifying categories of operator
#[derive(Debug)]
pub enum OperatorKind {
    // Mathematical operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Bitwise operators
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitLShift,
    BitRShift,
    // Logical operators
    Eq,
    And,
    Or,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Assignment operator
    Assign,
}

impl TryFrom<String> for OperatorKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            "+"  => Ok(Self::Add),
            "-"  => Ok(Self::Sub),
            "*"  => Ok(Self::Mul),
            "/"  => Ok(Self::Div),
            "%"  => Ok(Self::Mod),
            "&"  => Ok(Self::BitAnd),
            "|"  => Ok(Self::BitOr),
            "^"  => Ok(Self::BitXor),
            "~"  => Ok(Self::BitNot),
            "<<" => Ok(Self::BitLShift),
            ">>" => Ok(Self::BitRShift),
            "==" => Ok(Self::Eq),
            "&&" => Ok(Self::And),
            "||" => Ok(Self::Or),
            "<"  => Ok(Self::Less),
            ">"  => Ok(Self::Greater),
            "<=" => Ok(Self::LessEq),
            ">=" => Ok(Self::GreaterEq),
            "="  => Ok(Self::Assign),
            _    => Err(()),
        };
    }
}

/// A list specifying categories of literal
#[derive(Debug)]
pub enum LiteralKind {
    String(String),
    Integer(i64),
    Float(f64),
}
