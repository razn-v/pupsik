use crate::node::TreeNode;
use crate::token::{
    LiteralKind, OperatorKind, ReservedKind, SeparatorKind, Token,
};

/// Returns `result` if the current token is equal to `token`, otherwise throws
/// an error
macro_rules! expect_token {
    ($self:ident, $token:pat => $result:expr) => {
        match $self.current_token() {
            Some($token) => $result,
            _ => return Err(ParseError::UnexpectedToken),
        };
    };
}

/// The parser converts a given `input` to ASTs later used by the intermediate
/// code generator
pub struct Parser {
    input: Vec<Token>,
    /// Position of the parser in `input`
    pos: usize,
}

#[derive(Debug)]
pub enum ParseError {
    /// End of input
    EOF,
    /// The token was not the one expected
    UnexpectedToken,
}

type ParseResult = Result<Box<TreeNode>, ParseError>;

impl Parser {
    pub fn new(input: Vec<Token>) -> Self {
        Self { input, pos: 0 }
    }

    /// Returns the currently processed token
    fn current_token(&self) -> Option<&Token> {
        self.input.get(self.pos)
    }

    /// Returns true if `token` matches the current token
    fn match_token(&self, token: Token) -> bool {
        self.current_token() == Some(&token)
    }

    fn next_token(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.current_token()
    }

    /// Returns the next parsed top-level instruction. Top-level instructions
    /// are the root nodes of the program.
    pub fn next_toplevel(&mut self) -> ParseResult {
        // Functions, global vars, imports
        match self.current_token() {
            Some(Token::Reserved(ReservedKind::FunctionDecl)) => {
                self.parse_func()
            }
            None => Err(ParseError::EOF),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    /// Parses a function declaration and its content.
    fn parse_func(&mut self) -> ParseResult {
        // Skip function keyword
        self.next_token();

        let fn_name: String =
            expect_token!(self, Token::Identifier(x) => x.clone());
        self.next_token();

        expect_token!(self, Token::Separator(SeparatorKind::Colon) => ());
        self.next_token();

        // Function arguments, each argument is a tuple of its type and name
        let mut fn_args: Vec<(String, String)> = Vec::new();

        // While the current token is not an arrow, we have arguments left
        while !self.match_token(Token::Separator(SeparatorKind::Arrow)) {
            // If we hit a comma, we have another argument left
            if self.match_token(Token::Separator(SeparatorKind::Comma)) {
                // Skip comma
                self.next_token();
            }

            let arg_type: String =
                expect_token!(self, Token::Identifier(x) => x.clone());
            self.next_token();

            if arg_type == "Void" {
                // Void arguments have no name and can't be more than one
                expect_token!(self,
                    Token::Separator(SeparatorKind::Arrow) => ());
                break;
            }

            let arg_name: String =
                expect_token!(self, Token::Identifier(x) => x.clone());
            self.next_token();

            fn_args.push((arg_type, arg_name));
        }

        // Skip arrow
        self.next_token();

        // Function return type
        let fn_type = expect_token!(self, Token::Identifier(x) => x.clone());
        self.next_token();

        // Parse function content
        let fn_body = self.parse_block();
        if fn_body.is_err() {
            return Err(fn_body.unwrap_err());
        }

        Ok(Box::new(TreeNode::FunctionDecl {
            name: fn_name,
            args: fn_args,
            ret: fn_type,
            body: fn_body.unwrap(),
        }))
    }

    /// Parses a block. A block is a list of nodes.
    fn parse_block(&mut self) -> Result<Vec<Box<TreeNode>>, ParseError> {
        expect_token!(self, Token::Separator(SeparatorKind::OpenBrace) => ());
        self.next_token();

        // Loop over and parse each line (node) of the block
        let mut nodes = Vec::<Box<TreeNode>>::new();

        // While the current token is not a close brace, we have nodes left
        while !self.match_token(Token::Separator(SeparatorKind::CloseBrace)) {
            let node = self.parse_node();
            if node.is_err() {
                return Err(node.unwrap_err());
            }
            nodes.push(node.unwrap());
        }

        // Skip close brace
        self.next_token();
        Ok(nodes)
    }

    /// Parses a node. A node is a single line in a block, each node must end
    /// with a semicolon.
    fn parse_node(&mut self) -> ParseResult {
        let node = match self.current_token() {
            Some(Token::Reserved(ReservedKind::Return)) => self.parse_return(),
            _ => todo!(),
        };

        expect_token!(self, Token::Separator(SeparatorKind::Semicolon) => ());
        self.next_token();

        node
    }

    /// Parses an expression
    fn parse_expr(&mut self) -> ParseResult {
        let left_hand = self.parse_primary();
        if left_hand.is_err() {
            return left_hand;
        }

        let mut left_hand = left_hand.unwrap();
        self.parse_binop(0, &mut left_hand)
    }

    /// Parses either a literal, a call or a parenthesis expression
    pub fn parse_primary(&mut self) -> ParseResult {
        match self.current_token() {
            Some(Token::Literal(_)) => self.parse_literal(),
            Some(Token::Identifier(_)) => self.parse_call(),
            Some(Token::Separator(SeparatorKind::OpenParen)) => {
                self.parse_paren_expr()
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    /// Parses either a string, an integer, a float or a boolean
    fn parse_literal(&mut self) -> ParseResult {
        let literal = match self.current_token() {
            Some(Token::Literal(LiteralKind::String(val))) => {
                Ok(Box::new(TreeNode::String(val.clone())))
            }
            Some(Token::Literal(LiteralKind::Integer(val))) => {
                Ok(Box::new(TreeNode::Integer(val.clone())))
            }
            Some(Token::Literal(LiteralKind::Float(val))) => {
                Ok(Box::new(TreeNode::Float(val.clone())))
            }
            Some(Token::Literal(LiteralKind::Boolean(val))) => {
                Ok(Box::new(TreeNode::Boolean(val.clone())))
            }
            _ => Err(ParseError::UnexpectedToken),
        };

        self.next_token();
        literal
    }

    /// Parses either a variable call or a function call
    fn parse_call(&mut self) -> ParseResult {
        let name = expect_token!(self, Token::Identifier(x) => x.clone());
        self.next_token();

        // Check if we have a function call
        if self.match_token(Token::Separator(SeparatorKind::OpenParen)) {
            return self.parse_fn_call(name);
        }

        Ok(Box::new(TreeNode::VarCall(name)))
    }

    /// Parses a function call
    fn parse_fn_call(&mut self, name: String) -> ParseResult {
        // Skip open parenthesis
        self.next_token();

        // Parse arguments passed to the function call
        let mut args = Vec::<Box<TreeNode>>::new();

        // While the current token is not a close parenthesis, we have arguments
        // to parse
        while !self.match_token(Token::Separator(SeparatorKind::CloseParen)) {
            // If we hit a comma, we have another argument left
            if self.match_token(Token::Separator(SeparatorKind::Comma)) {
                // Skip comma
                self.next_token();
            }

            let arg = self.parse_expr();
            if arg.is_err() {
                return arg;
            }

            args.push(arg.unwrap());
        }

        // Skip close parenthesis
        self.next_token();

        Ok(Box::new(TreeNode::FunctionCall { name, args }))
    }

    /// Parses a parenthesis expression
    fn parse_paren_expr(&mut self) -> ParseResult {
        // Skip parenthesis
        self.next_token();

        let expr = self.parse_expr();
        if expr.is_err() {
            return expr;
        }
        // Make sure the expression ends
        expect_token!(self, Token::Separator(SeparatorKind::CloseParen) => ());
        self.next_token();

        expr
    }

    /// Parses a binary operation
    fn parse_binop(
        &mut self,
        expr_prec: isize,
        left_hand: &mut Box<TreeNode>,
    ) -> ParseResult {
        loop {
            // Make sure we have a binary operator, otherwise return left hand
            let binop: OperatorKind = match self.current_token() {
                Some(Token::Operator(op)) => op.clone(),
                _ => return Ok(left_hand.clone()),
            };

            let tok_prec = binop.get_precedence();
            if tok_prec < expr_prec {
                return Ok(left_hand.clone());
            }

            // Skip operator
            self.next_token();

            let right_hand = self.parse_primary();
            if right_hand.is_err() {
                return right_hand;
            }
            let mut right_hand = right_hand.unwrap();

            let next_prec: isize = match self.current_token() {
                Some(Token::Operator(op)) => op.get_precedence(),
                _ => -1,
            };
            if tok_prec < next_prec {
                right_hand =
                    match self.parse_binop(tok_prec + 1, &mut right_hand) {
                        Ok(binop) => binop,
                        Err(err) => return Err(err),
                    }
            }

            // Merge left hand and right hand
            *left_hand = Box::new(TreeNode::BinaryOp {
                op: binop,
                left: left_hand.clone(),
                right: right_hand,
            });
        }
    }

    /// Parses a return instruction
    fn parse_return(&mut self) -> ParseResult {
        // Skip return keyword
        self.next_token();

        let expr = self.parse_expr();
        if expr.is_err() {
            return expr;
        }

        Ok(Box::new(TreeNode::Return(expr.unwrap())))
    }
}

impl Iterator for Parser {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_toplevel() {
            Ok(func) => Some(Ok(func)),
            Err(ParseError::EOF) => None,
            Err(err) => panic!("{:?}", err),
        }
    }
}
