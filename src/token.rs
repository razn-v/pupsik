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
    BinaryOperator(BinaryKind),
    UnaryOperator(UnaryKind),
}

/// A list specifying categories of binary operator
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryKind {
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
    BitLShift,
    BitRShift,
    // Logical operators
    Eq,
    And,
    Or,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Other
    Assign,
    MemberAccess,
}

impl BinaryKind {
    pub fn get_precedence(&self) -> isize {
        match self {
            Self::Mul | Self::Div | Self::Mod => 1,
            Self::Add | Self::Sub => 2,
            Self::BitLShift | Self::BitRShift => 3,
            Self::Less | Self::Greater | Self::LessEq | Self::GreaterEq => 4,
            Self::Eq | Self::NotEq => 5,
            Self::BitAnd => 6,
            Self::BitXor => 7,
            Self::BitOr => 8,
            Self::And => 9,
            Self::Or => 10,
            _ => unimplemented!(),
        }
    }
}

/// A list specifying categories of unary operator
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryKind {
    BitNot,
    Not,
}

impl TryFrom<String> for OperatorKind {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        #[rustfmt::skip]
        return match value.as_ref() {
            "+"  => Ok(Self::BinaryOperator(BinaryKind::Add)),
            "-"  => Ok(Self::BinaryOperator(BinaryKind::Sub)),
            "*"  => Ok(Self::BinaryOperator(BinaryKind::Mul)),
            "/"  => Ok(Self::BinaryOperator(BinaryKind::Div)),
            "%"  => Ok(Self::BinaryOperator(BinaryKind::Mod)),
            "&"  => Ok(Self::BinaryOperator(BinaryKind::BitAnd)),
            "|"  => Ok(Self::BinaryOperator(BinaryKind::BitOr)),
            "^"  => Ok(Self::BinaryOperator(BinaryKind::BitXor)),
            "~"  => Ok(Self::UnaryOperator(UnaryKind::BitNot)),
            "<<" => Ok(Self::BinaryOperator(BinaryKind::BitLShift)),
            ">>" => Ok(Self::BinaryOperator(BinaryKind::BitRShift)),
            "==" => Ok(Self::BinaryOperator(BinaryKind::Eq)),
            "&&" => Ok(Self::BinaryOperator(BinaryKind::And)),
            "||" => Ok(Self::BinaryOperator(BinaryKind::Or)),
            "!"  => Ok(Self::UnaryOperator(UnaryKind::Not)),
            "!=" => Ok(Self::BinaryOperator(BinaryKind::NotEq)),
            "<"  => Ok(Self::BinaryOperator(BinaryKind::Less)),
            ">"  => Ok(Self::BinaryOperator(BinaryKind::Greater)),
            "<=" => Ok(Self::BinaryOperator(BinaryKind::LessEq)),
            ">=" => Ok(Self::BinaryOperator(BinaryKind::GreaterEq)),
            "="  => Ok(Self::BinaryOperator(BinaryKind::Assign)),
            "."  => Ok(Self::BinaryOperator(BinaryKind::MemberAccess)),
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
