use std::convert::TryFrom;

use crate::token::{
    LiteralKind, OperatorKind, ReservedKind, SeparatorKind, Token,
};

/// The lexer convert a given `input` to tokens later used by the parser.
/// It is the verify first process of our compiler.
pub struct Lexer {
    input: String,
    /// Position of the lexer in `input`
    pos: usize,
    /// Position of the current token in `input`
    token_pos: usize,
}

/// A list specifying categories of lexer error
#[derive(Debug)]
pub enum LexerError {
    /// End of file/input
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

type LexerResult = Result<Token, LexerError>;

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            pos: 0,
            token_pos: 0,
        }
    }

    /// Returns the currently processed character
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    /// Returns the value of the currently processed token
    fn current_token(&self) -> String {
        self.input[self.token_pos..self.pos].to_string()
    }

    /// Returns the next character and increases the position of the lexer if
    /// `update` is true
    fn next_char(&mut self, update: bool) -> Option<char> {
        if update {
            self.pos += 1;
            return self.current_char();
        }
        self.input.chars().nth(self.pos + 1)
    }

    /// Returns the next lexed token
    fn next_token(&mut self) -> LexerResult {
        // Unlike `pos`, `token_pos` will not be updated until the next token,
        // thus allow us to know the start and the end of the token.
        self.token_pos = self.pos;

        if self.is_eof() {
            return Err(LexerError::EOF);
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

        Err(LexerError::InvalidToken)
    }

    /// Lex current chars into an identifier or a reserved keyword
    fn consume_keyword(&mut self) -> LexerResult {
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
        }

        Ok(Token::Identifier(self.current_token()))
    }

    /// Lex current chars into a separator
    fn consume_separator(&mut self) -> LexerResult {
        // Skip separator
        self.next_char(true);

        // If the separator is an arrow
        if self.current_char() == Some('>') {
            self.next_char(true);
        }

        let kind = SeparatorKind::try_from(self.current_token()).unwrap();
        Ok(Token::Separator(kind))
    }

    /// Lex current chars into an operator
    fn consume_operator(&mut self) -> LexerResult {
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

    /// Lex current chars into a string
    fn consume_string(&mut self) -> LexerResult {
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
                return Err(LexerError::InvalidString);
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
                .replace("\\\"", "\""),
        );

        Ok(Token::Literal(kind))
    }

    /// Lex current chars into either a float or an integer
    fn consume_number(&mut self) -> LexerResult {
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
            println!("{}", self.current_token());
            let float = match self.current_token().parse::<f64>() {
                Ok(f) => f,
                Err(_) => return Err(LexerError::InvalidFloat),
            };
            kind = LiteralKind::Float(float);
        } else {
            let int = match self.current_token().parse::<i64>() {
                Ok(i) => i,
                Err(_) => return Err(LexerError::InvalidInteger),
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

    /// Returns true if the current char is a letter from the alphabet
    fn is_letter(&self) -> bool {
        self.current_char().unwrap().is_ascii_alphabetic()
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
    /// bitwise operator, a logical operator or an assignment operator
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

impl Iterator for Lexer {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => return Some(Ok(token)),
            Err(LexerError::EOF) => return None,
            Err(err) => panic!("{:?}", err),
        }
    }
}
