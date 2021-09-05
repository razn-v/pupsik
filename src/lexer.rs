use crate::error::CompileError;
use crate::token::{
    LiteralKind, OperatorKind, ReservedKind, SeparatorKind, Token,
};
use crate::TraceInfo;

use std::convert::TryFrom;
use std::ops::Deref;

/// The lexer converts a given `input` to tokens later used by the parser.
/// It is the verify first process of our compiler.
pub struct Lexer<'a> {
    input: &'a Vec<TraceInfo<char>>,
    /// Position of the lexer in `input`
    pos: usize,
    /// Position of the current token in `input`
    token_pos: usize,
}

/// A list specifying categories of lex error
#[derive(Debug)]
pub enum LexError {
    /// End of input
    EOF,
    /// The token couldn't be recognized
    InvalidToken,
    /// The string had no end (missing last quote)
    InvalidString,
    /// The token couldn't be parsed as an integer
    InvalidInteger,
    /// The token couldn't be parsed as a float
    InvalidFloat,
}

impl CompileError for LexError {
    fn error_msg(&self) -> String {
        match self {
            LexError::InvalidToken => {
                "This character couldn't be recognized as a valid token"
            }
            LexError::InvalidString => {
                "This string has no end. Did you forget a quote?"
            }
            LexError::InvalidInteger => "Couldn't parse this as an integer",
            LexError::InvalidFloat => "Couldn't parse this as a float",
            LexError::EOF => unreachable!(),
        }
        .into()
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

type LexResult = Result<Token, LexError>;

impl<'a> Lexer<'a> {
    pub fn new(input: &'a Vec<TraceInfo<char>>) -> Self {
        Self {
            input,
            pos: 0,
            token_pos: 0,
        }
    }

    /// Get trace info of `value`
    fn get_trace<T>(&self, value: T) -> TraceInfo<T> {
        // In certains cases, the position of the lexer will be at the character
        // that couldn't be lexed, this is why we also need to check the
        // calculated length later one
        let char_info = match self.input.get(self.token_pos) {
            Some(info) => info,
            None => self.input.get(self.token_pos - 1).unwrap(),
        };

        // Make sure the length is not zero
        let length = match self.pos - self.token_pos {
            0 => 1,
            n => n,
        };

        TraceInfo::new(value, char_info.n_line, char_info.pos, length)
    }

    /// Returns the currently processed character
    fn current_char(&self) -> Option<char> {
        self.input.get(self.pos).map(|x| *x.deref())
    }

    /// Returns the value of the currently processed token
    fn current_token(&self) -> String {
        self.input
            [self.token_pos..self.pos + (self.token_pos == self.pos) as usize]
            .into_iter()
            .map(|x| x.deref())
            .collect()
    }

    /// Returns the next character and increases the position of the lexer if
    /// `update` is true
    fn next_char(&mut self, update: bool) -> Option<char> {
        if update {
            self.pos += 1;
            return self.current_char();
        }
        self.input.get(self.pos + 1).map(|x| *x.deref())
    }

    /// Returns the next lexed token
    fn next_token(&mut self) -> LexResult {
        // Unlike `pos`, `token_pos` will not be updated until the next token,
        // thus allow us to know the start and the end of the token.
        self.token_pos = self.pos;

        if self.is_eof() {
            return Err(LexError::EOF);
        } else if self.is_space() {
            // Skip space
            self.next_char(true);
            return self.next_token();
        } else if self.is_letter() {
            return self.consume_keyword();
        } else if self.is_separator() {
            return self.consume_separator();
        } else if self.is_operator() {
            return self.consume_operator();
        } else if self.is_string() {
            return self.consume_string();
        } else if self.is_digit() {
            return self.consume_number();
        }

        Err(LexError::InvalidToken)
    }

    /// Lexes current chars into an identifier, a reserved keyword or a boolean
    fn consume_keyword(&mut self) -> LexResult {
        // Get the next char until the end of the keyword
        while self.is_letter() || self.is_digit() {
            if self.next_char(true).is_none() {
                // End of the keyword
                break;
            }
        }

        // If the keyword is reserved
        if let Ok(kind) = ReservedKind::try_from(self.current_token()) {
            return Ok(Token::Reserved(kind));
        } else {
            // Check if the keyword is a boolean
            match self.current_token().as_ref() {
                "true" => {
                    return Ok(Token::Literal(LiteralKind::Boolean(true)))
                }
                "false" => {
                    return Ok(Token::Literal(LiteralKind::Boolean(false)))
                }
                _ => {}
            }
        }

        Ok(Token::Identifier(self.current_token()))
    }

    /// Lexes current chars into a separator
    fn consume_separator(&mut self) -> LexResult {
        // Skip separator
        self.next_char(true);

        // If the separator is an arrow
        if self.current_char() == Some('>') {
            self.next_char(true);
        }

        let kind = SeparatorKind::try_from(self.current_token()).unwrap();
        Ok(Token::Separator(kind))
    }

    /// Lexes current chars into an operator
    fn consume_operator(&mut self) -> LexResult {
        let previous_char = self.current_char().unwrap();

        // Skip first character and check for 2-characters long operator
        if let Some(chr) = self.next_char(true) {
            if OperatorKind::try_from(format!("{}{}", previous_char, chr))
                .is_ok()
            {
                self.next_char(true);
            }
        }

        let kind = OperatorKind::try_from(self.current_token()).unwrap();
        Ok(Token::Operator(kind))
    }

    /// Lexes current chars into a string
    fn consume_string(&mut self) -> LexResult {
        // Skip first quote
        self.next_char(true);

        // Get the next char until the end of the string
        while self.current_char() != Some('"') {
            // Support for escape sequences
            if self.current_char() == Some('\\')
                && self.next_char(true) == Some('"')
            {
                self.next_char(true);
            } else if self.current_char().is_none() {
                // We hit the end of the input without finding the end of the
                // string
                return Err(LexError::InvalidString);
            } else {
                self.next_char(true);
            }
        }

        // Skip last quote
        self.next_char(true);

        // Ignore quotes and escape sequences
        let string = self.current_token();
        let kind = LiteralKind::String(
            string[1..string.len() - 1]
                .to_string()
                .replace("\\\"", "\"")
                .replace("\\n", "\n"),
        );

        Ok(Token::Literal(kind))
    }

    /// Lexes current chars into either a float or an integer
    fn consume_number(&mut self) -> LexResult {
        // Get the next char until the end of the number
        while self.is_digit() || self.current_char() == Some('.') {
            if self.next_char(true).is_none() {
                // End of the number
                break;
            }
        }

        // Parse integer/float
        let kind;
        if self.current_token().contains('.') {
            let float = match self.current_token().parse::<f64>() {
                Ok(f) => f,
                Err(_) => return Err(LexError::InvalidFloat),
            };
            kind = LiteralKind::Float(float);
        } else {
            let int = match self.current_token().parse::<i64>() {
                Ok(i) => i,
                Err(_) => return Err(LexError::InvalidInteger),
            };
            kind = LiteralKind::Integer(int);
        }

        Ok(Token::Literal(kind))
    }

    /// Returns true if the current char is none, meaning that we have no
    /// characters left
    fn is_eof(&self) -> bool {
        self.current_char().is_none()
    }

    /// Returns true if the current char is a space, spaces are not tokenized
    /// by the lexer but rather ignored
    fn is_space(&self) -> bool {
        self.current_char().unwrap() == ' '
    }

    /// Returns true if the current char is a letter from the alphabet or is an
    /// underscore
    fn is_letter(&self) -> bool {
        let chr = self.current_char().unwrap();
        chr.is_ascii_alphabetic() || chr == '_'
    }

    /// Returns true if the current char is a separator
    fn is_separator(&mut self) -> bool {
        // Check if the separator is an arrow
        if self.current_char().unwrap() == '-' {
            if self.next_char(false) == Some('>') {
                return true;
            }
        }

        SeparatorKind::try_from(self.current_char().unwrap().to_string())
            .is_ok()
    }

    /// Returns true if the current char is either a mathematical operator, a
    /// bitwise operator, a logical operator or more
    fn is_operator(&self) -> bool {
        // Because we don't have an operator whom the first character is not a
        // valid operator in our language, we don't need the remaining
        // characters to assert that the current token is an operator.
        OperatorKind::try_from(self.current_char().unwrap().to_string()).is_ok()
    }

    /// Returns true if the current char is a quote
    fn is_string(&self) -> bool {
        self.current_char().unwrap() == '"'
    }

    /// Returns true if the current char is a number from 0 to 9 (included)
    fn is_digit(&self) -> bool {
        self.current_char().unwrap().is_ascii_digit()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TraceInfo<Token>, TraceInfo<LexError>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Err(LexError::EOF) => None,
            Ok(token) => Some(Ok(self.get_trace(token))),
            Err(err) => Some(Err(self.get_trace(err))),
        }
    }
}
