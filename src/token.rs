use std::convert::TryFrom;

/// Tokens are high-level identifiers used by the parser to generate ASTs
#[derive(Debug, Clone, PartialEq)]
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A list specifying categories of reserved keyword
#[derive(Debug, Clone, PartialEq)]
pub enum ReservedKind {
    VariableType(TypeKind),
    FunctionDecl,
    Return,
    If,
    Else,
    Let,
}

impl TryFrom<String> for ReservedKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            "void"   => Ok(Self::VariableType(TypeKind::Void)),
            "int64"  => Ok(Self::VariableType(TypeKind::Int64)),
            "bool"   => Ok(Self::VariableType(TypeKind::Bool)),
            "str"    => Ok(Self::VariableType(TypeKind::Str)),
            "fnc"    => Ok(Self::FunctionDecl),
            "return" => Ok(Self::Return),
            "if"     => Ok(Self::If),
            "else"   => Ok(Self::Else),
            "let"    => Ok(Self::Let),
            _        => Err(()),
        };
    }
}

/// A list specifying categories of variable type
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Void,
    Int64,
    Bool,
    Str,
}

/// A list specifying categories of separator
#[derive(Debug, Clone, PartialEq)]
pub enum SeparatorKind {
    Comma,
    Colon,
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Arrow,
    At,
}

impl TryFrom<String> for SeparatorKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            ","  => Ok(Self::Comma),
            ":"  => Ok(Self::Colon),
            ";"  => Ok(Self::Semicolon),
            "("  => Ok(Self::OpenParen),
            ")"  => Ok(Self::CloseParen),
            "{"  => Ok(Self::OpenBrace),
            "}"  => Ok(Self::CloseBrace),
            "->" => Ok(Self::Arrow),
            "@"  => Ok(Self::At),
            _    => Err(()),
        };
    }
}

/// A list specifying categories of operator
#[derive(Debug, Clone, PartialEq)]
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
    Not,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Other
    Assign,
    MemberAccess,
}

impl OperatorKind {
    pub fn get_precedence(&self) -> isize {
        match self {
            Self::Not | Self::BitNot => 1,
            Self::Mul | Self::Div | Self::Mod => 2,
            Self::Add | Self::Sub => 3,
            Self::BitLShift | Self::BitRShift => 4,
            Self::Less | Self::Greater | Self::LessEq | Self::GreaterEq => 5,
            Self::Eq | Self::NotEq => 6,
            Self::BitAnd => 7,
            Self::BitXor => 8,
            Self::BitOr => 9,
            Self::And => 10,
            Self::Or => 11,
            _ => unimplemented!(),
        }
    }
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
            "!"  => Ok(Self::Not),
            "!=" => Ok(Self::NotEq),
            "<"  => Ok(Self::Less),
            ">"  => Ok(Self::Greater),
            "<=" => Ok(Self::LessEq),
            ">=" => Ok(Self::GreaterEq),
            "="  => Ok(Self::Assign),
            "."  => Ok(Self::MemberAccess),
            _    => Err(()),
        };
    }
}

/// A list specifying categories of literal
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
}
