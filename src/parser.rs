use crate::error::CompileError;
use crate::node::TreeNode;
use crate::token::{
    LiteralKind, OperatorKind, ReservedKind, SeparatorKind, Token, TypeKind,
};
use crate::{unwrap_or_return, TraceInfo};

use std::ops::Deref;

/// Returns `result` if the current token type is equal to `type`, otherwise
/// throws an error
macro_rules! expect_token {
    // Error not specified
    ($self:ident, $type:pat => $result:expr) => {
        match $self.current_token() {
            Some($type) => $result,
            _ => return Err($self.get_trace(ParseError::UnexpectedToken)),
        };
    };

    // Error specified
    ($self:ident, $error:expr, $type:pat => $result:expr) => {
        match $self.current_token() {
            Some($type) => $result,
            _ => return Err($self.get_trace($error)),
        };
    };
}

/// The parser converts a given `input` to ASTs later used by the intermediate
/// code generator
pub struct Parser<'a> {
    input: &'a Vec<TraceInfo<Token>>,
    /// Position of the parser in `input`
    pos: usize,
    /// Position of the current node parsed in `input`
    node_pos: usize,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    /// End of input
    EOF,
    /// The token was not the one expected
    UnexpectedToken,
    /// Expected a char but got something else
    ExpectedChar(char),
    /// Expected an identifier but got something else
    ExpectedIdentifier,
    /// Expected an arrow but got something else
    ExpectedArrow,
    /// Expected a type but got something else
    ExpectedType,
}

impl CompileError for ParseError {
    fn error_msg(&self) -> String {
        let tmp;

        match self {
            ParseError::UnexpectedToken => "This token was not expected",
            ParseError::ExpectedChar(chr) => {
                tmp = format!("Expected '{}' after this", chr);
                &tmp
            }
            ParseError::ExpectedIdentifier => {
                "Expected an identifier after this"
            }
            ParseError::ExpectedArrow => "Expected '->' after this",
            ParseError::ExpectedType => "Expected a valid type after this",
            ParseError::EOF => unreachable!(),
        }
        .into()
    }
}

type ParseResult = Result<TraceInfo<Box<TreeNode>>, TraceInfo<ParseError>>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a Vec<TraceInfo<Token>>) -> Self {
        Self {
            input,
            pos: 0,
            node_pos: 0,
        }
    }

    /// Get trace info of `value`
    fn get_trace<T>(&self, value: T) -> TraceInfo<T> {
        let token_info = match self.input.get(self.node_pos) {
            Some(info) => info,
            None => self.input.get(self.node_pos - 1).unwrap(),
        };

        // Calculate the length of the node by summing each token part of it
        let length: usize = self.input[self.node_pos..self.pos]
            .into_iter()
            .map(|x| x.len)
            .sum();

        TraceInfo::new(value, token_info.n_line, token_info.pos, length)
    }

    /// Returns the currently processed token
    fn current_token(&self) -> Option<&Token> {
        self.input.get(self.pos).map(|x| x.deref())
    }

    /// Returns true if `token_type` matches the current token type
    fn match_token(&self, token_type: Token) -> bool {
        match self.current_token() {
            Some(token) => token == &token_type,
            _ => false,
        }
    }

    /// Returns the next token and updates position by one
    fn next_token(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.current_token()
    }

    /// Returns the next parsed top-level instruction. Top-level instructions
    /// are the root nodes of the program.
    pub fn next_toplevel(&mut self) -> ParseResult {
        self.node_pos = self.pos;

        // Functions, global vars, imports
        match self.current_token() {
            Some(Token::Reserved(ReservedKind::FunctionDecl)) => {
                self.parse_func()
            }
            None => Err(self.get_trace(ParseError::EOF)),
            _ => Err(self.get_trace(ParseError::UnexpectedToken)),
        }
    }

    /// Parses a function declaration and its content.
    fn parse_func(&mut self) -> ParseResult {
        // Skip function keyword
        self.next_token();

        let fn_name: String = expect_token!(self,
            ParseError::ExpectedIdentifier, Token::Identifier(x) => x.clone());
        self.next_token();

        expect_token!(self, ParseError::ExpectedChar(':'),
            Token::Separator(SeparatorKind::Colon) => ());
        self.next_token();

        // Function arguments, each argument is a tuple of its type and name
        let mut fn_args: Vec<(TypeKind, String)> = Vec::new();

        // While the current token is not an arrow, we have arguments left
        while !self.match_token(Token::Separator(SeparatorKind::Arrow)) {
            // If we hit a comma, we have another argument left
            if self.match_token(Token::Separator(SeparatorKind::Comma)) {
                // Skip comma
                self.next_token();
            }

            let arg_type: TypeKind = expect_token!(self,
                ParseError::ExpectedType,
                Token::Reserved(ReservedKind::VariableType(x)) => x.clone());
            self.next_token();

            if arg_type == TypeKind::Void {
                // Void arguments have no name and can't be more than one
                expect_token!(self, ParseError::ExpectedArrow,
                    Token::Separator(SeparatorKind::Arrow) => ());
                break;
            }

            let arg_name: String = expect_token!(self,
                ParseError::ExpectedIdentifier,
                Token::Identifier(x) => x.clone());
            self.next_token();

            fn_args.push((arg_type, arg_name));
        }

        // Skip arrow
        self.next_token();

        // Function return type
        let fn_type: TypeKind = expect_token!(self, ParseError::ExpectedType,
            Token::Reserved(ReservedKind::VariableType(x)) => x.clone());
        self.next_token();

        // Parse function content
        let fn_body = unwrap_or_return!(self.parse_block());

        Ok(self.get_trace(Box::new(TreeNode::FunctionDecl {
            name: fn_name,
            args: fn_args,
            ret: fn_type,
            body: fn_body,
        })))
    }

    /// Parses a block. A block is a list of nodes.
    fn parse_block(
        &mut self,
    ) -> Result<Vec<TraceInfo<Box<TreeNode>>>, TraceInfo<ParseError>> {
        expect_token!(self, ParseError::ExpectedChar('{'),
            Token::Separator(SeparatorKind::OpenBrace) => ());
        self.next_token();

        // Loop over and parse each line (node) of the block
        let mut nodes = Vec::<TraceInfo<Box<TreeNode>>>::new();

        // While the current token is not a close brace, we have nodes left
        while !self.match_token(Token::Separator(SeparatorKind::CloseBrace)) {
            let node = unwrap_or_return!(self.parse_node());
            nodes.push(node);
        }

        // Skip close brace
        self.next_token();
        Ok(nodes)
    }

    /// Parses a node. A node is a single line in a block, each node must end
    /// with a semicolon.
    fn parse_node(&mut self) -> ParseResult {
        let pos_backup = self.node_pos;
        self.node_pos = self.pos;

        let mut node = unwrap_or_return!(match self.current_token() {
            Some(Token::Reserved(ReservedKind::Return)) => {
                self.parse_return()
            }
            Some(Token::Reserved(ReservedKind::If)) => {
                self.parse_if_expr()
            }
            Some(Token::Reserved(ReservedKind::Let)) => {
                self.parse_var_decl()
            }
            Some(Token::Identifier(_)) => self.parse_call(),
            _ => todo!(),
        });

        expect_token!(self, ParseError::ExpectedChar(';'),
            Token::Separator(SeparatorKind::Semicolon) => ());
        self.next_token();

        node = self.get_trace((*node).clone());

        self.node_pos = pos_backup;
        Ok(node)
    }

    /// Parses an expression
    fn parse_expr(&mut self) -> ParseResult {
        let pos_backup = self.node_pos;
        self.node_pos = self.pos;

        let mut left_hand = unwrap_or_return!(self.parse_primary());
        let ret = self.parse_binop(0, &mut left_hand);

        self.node_pos = pos_backup;
        ret
    }

    /// Parses either a literal, a call or a parenthesis expression
    pub fn parse_primary(&mut self) -> ParseResult {
        let pos_backup = self.node_pos;
        self.node_pos = self.pos;

        let ret = match self.current_token() {
            Some(Token::Literal(_)) => self.parse_literal(),
            Some(Token::Identifier(_)) => self.parse_call(),
            Some(Token::Separator(SeparatorKind::OpenParen)) => {
                self.parse_paren_expr()
            }
            _ => Err(self.get_trace(ParseError::UnexpectedToken)),
        };

        self.node_pos = pos_backup;
        ret
    }

    /// Parses either a string, an integer, a float or a boolean
    fn parse_literal(&mut self) -> ParseResult {
        let literal = match self.current_token() {
            Some(Token::Literal(LiteralKind::String(val))) => {
                TreeNode::String(val.clone())
            }
            Some(Token::Literal(LiteralKind::Integer(val))) => {
                TreeNode::Integer(val.clone())
            }
            Some(Token::Literal(LiteralKind::Float(val))) => {
                TreeNode::Float(val.clone())
            }
            Some(Token::Literal(LiteralKind::Boolean(val))) => {
                TreeNode::Boolean(val.clone())
            }
            _ => unreachable!(),
        };

        self.next_token();
        Ok(self.get_trace(Box::new(literal)))
    }

    /// Parses either a variable call or a function call
    fn parse_call(&mut self) -> ParseResult {
        let name = expect_token!(self,
            ParseError::ExpectedIdentifier, Token::Identifier(x) => x.clone());
        self.next_token();

        // Check if we have a function call
        if self.match_token(Token::Separator(SeparatorKind::OpenParen)) {
            return self.parse_fn_call(name);
        }

        Ok(self.get_trace(Box::new(TreeNode::VarCall(name))))
    }

    /// Parses a function call
    fn parse_fn_call(&mut self, name: String) -> ParseResult {
        // Skip open parenthesis
        self.next_token();

        // Parse arguments passed to the function call
        let mut args = Vec::<TraceInfo<Box<TreeNode>>>::new();

        // While the current token is not a close parenthesis, we have arguments
        // to parse
        while !self.match_token(Token::Separator(SeparatorKind::CloseParen)) {
            // If we hit a comma, we have another argument left
            if self.match_token(Token::Separator(SeparatorKind::Comma)) {
                // Skip comma
                self.next_token();
            }

            let arg = unwrap_or_return!(self.parse_expr());
            args.push(arg);
        }

        // Skip close parenthesis
        self.next_token();

        Ok(self.get_trace(Box::new(TreeNode::FunctionCall { name, args })))
    }

    /// Parses a parenthesis expression
    fn parse_paren_expr(&mut self) -> ParseResult {
        // Skip parenthesis
        self.next_token();

        let expr = *unwrap_or_return!(self.parse_expr()).deref().clone();

        // Make sure the expression ends
        expect_token!(self, ParseError::ExpectedChar(')'),
            Token::Separator(SeparatorKind::CloseParen) => ());
        self.next_token();

        Ok(self.get_trace(Box::new(expr)))
    }

    /// Parses a binary operation
    fn parse_binop(
        &mut self,
        expr_prec: isize,
        left_hand: &mut TraceInfo<Box<TreeNode>>,
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
            let mut right_hand = unwrap_or_return!(self.parse_primary());

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
            *left_hand = self.get_trace(Box::new(TreeNode::BinaryOp {
                operator: binop,
                left: left_hand.clone(),
                right: right_hand,
            }));
        }
    }

    /// Parses a return instruction
    fn parse_return(&mut self) -> ParseResult {
        // Skip return keyword
        self.next_token();
        let expr =
            if self.match_token(Token::Separator(SeparatorKind::Semicolon)) {
                None
            } else {
                Some(unwrap_or_return!(self.parse_expr()))
            };

        Ok(self.get_trace(Box::new(TreeNode::Return(expr))))
    }

    /// Parses an if expression
    fn parse_if_expr(&mut self) -> ParseResult {
        // Skip if keyword
        self.next_token();

        let cond = unwrap_or_return!(self.parse_expr());
        let then_body = unwrap_or_return!(self.parse_block());

        // Check if else is present
        let mut else_body = Vec::new();
        if self.match_token(Token::Reserved(ReservedKind::Else)) {
            self.next_token();
            else_body = unwrap_or_return!(self.parse_block());
        }

        Ok(self.get_trace(Box::new(TreeNode::Condition {
            cond,
            then_body,
            else_body,
        })))
    }

    /// Parses a variable declaration
    fn parse_var_decl(&mut self) -> ParseResult {
        // Skip let keyword
        self.next_token();

        // Get variable name
        let name = expect_token!(self,
            ParseError::ExpectedIdentifier, Token::Identifier(x) => x.clone());
        self.next_token();

        // Make sure we have an assign operator
        expect_token!(self, Token::Operator(OperatorKind::Assign) => ());
        self.next_token();

        // Parse variable value
        let value = unwrap_or_return!(self.parse_expr());
        Ok(self.get_trace(Box::new(TreeNode::VariableDecl { name, value })))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_toplevel() {
            Err(err) if err.deref() == &ParseError::EOF => None,
            res => Some(res),
        }
    }
}
