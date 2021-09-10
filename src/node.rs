use crate::token::{BinaryKind, TypeKind, UnaryKind};
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
        operator: BinaryKind,
        left: TraceInfo<Box<TreeNode>>,
        right: TraceInfo<Box<TreeNode>>,
    },

    UnaryOp {
        operator: UnaryKind,
        value: TraceInfo<Box<TreeNode>>,
    },

    Return(Option<TraceInfo<Box<TreeNode>>>),

    Condition {
        cond: TraceInfo<Box<TreeNode>>,
        then_body: NodeBlock,
        else_body: NodeBlock,
    },

    VariableDecl {
        name: String,
        var_type: Option<TypeKind>,
        value: Option<TraceInfo<Box<TreeNode>>>,
    },

    VarAssign {
        name: String,
        value: TraceInfo<Box<TreeNode>>,
    },

    ForLoop {
        var_name: String,
        var_val: TraceInfo<Box<TreeNode>>,
        // Condition to meet to stop the loop
        cond: TraceInfo<Box<TreeNode>>,
        // Assignment executed at each iteration
        assign: TraceInfo<Box<TreeNode>>,
        body: NodeBlock,
    },

    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),

    VarCall(String),

    FunctionCall {
        name: String,
        args: Vec<TraceInfo<Box<TreeNode>>>,
        is_extern: bool,
    },
}
