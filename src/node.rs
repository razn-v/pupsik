use crate::token::{OperatorKind, TypeKind};
use crate::TraceInfo;

type NodeBlock = Vec<TraceInfo<Box<TreeNode>>>;

/// A list specifying categories of instructions and types used by the language
#[derive(Debug, Clone)]
pub enum TreeNode {
    FunctionDecl {
        name: String,
        // (argument type, argument name)
        args: Vec<(TypeKind, String)>,
        ret: TypeKind,
        body: NodeBlock,
    },

    BinaryOp {
        operator: OperatorKind,
        left: TraceInfo<Box<TreeNode>>,
        right: TraceInfo<Box<TreeNode>>,
    },

    Return(Option<TraceInfo<Box<TreeNode>>>),
    Condition {
        cond: TraceInfo<Box<TreeNode>>,
        then_body: NodeBlock,
        else_body: NodeBlock,
    },
    VariableDecl {
        name: String,
        value: TraceInfo<Box<TreeNode>>,
    },

    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),

    VarCall(String),
    FunctionCall {
        name: String,
        args: Vec<TraceInfo<Box<TreeNode>>>,
    },
}
