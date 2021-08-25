use crate::token::OperatorKind;
use std::fmt::Debug;

/// A list specifying categories of instructions and types used by the language
#[derive(Debug, Clone)]
pub enum TreeNode {
    FunctionDecl {
        name: String,
        // (argument type, argument name)
        args: Vec<(String, String)>,
        ret: String,
        body: Vec<Box<TreeNode>>,
    },

    BinaryOp {
        op: OperatorKind,
        left: Box<TreeNode>,
        right: Box<TreeNode>,
    },

    Return(Box<TreeNode>),

    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),

    VarCall(String),
    FunctionCall {
        name: String,
        args: Vec<Box<TreeNode>>,
    },
}
