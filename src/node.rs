use crate::token::{BinaryKind, TypeKind, UnaryKind};
use crate::TraceInfo;

type NodeBlock = Vec<TracedNode>;
pub type TracedNode = TraceInfo<Box<TreeNode>>;

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
        left: TracedNode,
        right: TracedNode,
    },

    UnaryOp {
        operator: UnaryKind,
        value: TracedNode,
    },

    Return(Option<TracedNode>),

    Condition {
        cond: TracedNode,
        then_body: NodeBlock,
        else_body: NodeBlock,
    },

    VariableDecl {
        name: String,
        var_type: Option<TypeKind>,
        value: Option<TracedNode>,
    },

    VarAssign {
        name: String,
        value: TracedNode,
    },

    ForLoop {
        var: TracedNode,
        // Condition to meet to stop the loop
        cond: TracedNode,
        // Assignment executed at each iteration
        assign: TracedNode,
        body: NodeBlock,
    },

    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),

    VarCall(String),

    FunctionCall {
        name: String,
        args: Vec<TracedNode>,
        is_extern: bool,
    },
}
