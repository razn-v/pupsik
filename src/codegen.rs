use crate::error::CompileError;
use crate::node::TreeNode;
use crate::token::{OperatorKind, TypeKind};
use crate::{unwrap_or_return, TraceInfo};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target,
    TargetMachine, TargetTriple,
};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue,
};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::collections::HashMap;
use std::ops::Deref;
use std::path::Path;
use std::process::Command;

/// Check if an error was found by LLVM
macro_rules! check_error {
    ($self:ident, $node:ident) => {
        if $self.module.verify().is_err() {
            return Err($self.get_trace(
                CodegenError::ModuleError($self.module_error()),
                $node,
            ));
        }
    };
}

/// The intermediate code generator generates LLVM IR which is then compiled
/// into an object file and finally linked with gcc to produce the final program
pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    /// Function pass manager, used to optimize LLVM IR
    fpm: PassManager<FunctionValue<'ctx>>,
    /// Host machine, used to write files
    target_machine: TargetMachine,
    /// Stored variables
    variables: HashMap<String, PointerValue<'ctx>>,
}

#[derive(Debug)]
pub enum CodegenError {
    /// Variable/function name already used
    NameAlreadyUsed,
    /// Referenced variable not found
    VariableNotFound,
    /// Referenced function not found
    FunctionNotFound,
    /// Operands type in binary operation not supported
    InvalidOperation,
    /// The function was used in an expression but returns nothing
    VoidFunction,
    /// LLVM error
    ModuleError(String),
}

impl CompileError for CodegenError {
    fn error_msg(&self) -> String {
        match self {
            CodegenError::NameAlreadyUsed => {
                "This name is already used elsewhere"
            }
            CodegenError::VariableNotFound => {
                "This variable was not found. Did you miswrite it?"
            }
            CodegenError::FunctionNotFound => {
                "No function with this name was found. Did you miswrite it?"
            }
            CodegenError::InvalidOperation => {
                "Operands type not supported in this operation"
            }
            CodegenError::VoidFunction => {
                "This function is used in an expression but returns nothing"
            }
            CodegenError::ModuleError(msg) => msg,
        }
        .into()
    }
}

type CodegenResult<T> = Result<T, TraceInfo<CodegenError>>;

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module: &str) -> Self {
        let module = context.create_module(module);
        let builder = context.create_builder();

        // Enable function optimization
        let fpm = PassManager::create(&module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.initialize();

        // Initialize target config
        Target::initialize_x86(&InitializationConfig::default());
        let target = Target::from_name("x86-64").unwrap();

        // Create target machine
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        Self {
            context,
            module,
            builder,
            fpm,
            target_machine,
            variables: HashMap::new(),
        }
    }

    /// Get trace info of `value`
    fn get_trace<T>(
        &self,
        value: T,
        node: &TraceInfo<Box<TreeNode>>,
    ) -> TraceInfo<T> {
        TraceInfo::new(value, node.n_line, node.pos, node.len)
    }

    /// Get LLVM module error
    fn module_error(&self) -> String {
        self.module
            .verify()
            .unwrap_err()
            .to_string()
            .split("\n")
            .next()
            .unwrap()
            .to_string()
    }

    /// Returns a LLVM basic type from a type `kind`
    fn get_type(
        &self,
        kind: &TypeKind,
    ) -> Result<Option<BasicTypeEnum<'ctx>>, CodegenError> {
        return match kind {
            TypeKind::Void => Ok(None),
            TypeKind::Int64 => {
                Ok(Some(self.context.i64_type().as_basic_type_enum()))
            }
            TypeKind::Bool => {
                Ok(Some(self.context.bool_type().as_basic_type_enum()))
            }
        };
    }

    /// Compiles a given function's content and prototype
    pub fn compile_func(
        &mut self,
        func: &TraceInfo<Box<TreeNode>>,
    ) -> CodegenResult<FunctionValue> {
        // Extract function attributes from the node
        let (fn_name, fn_args, fn_ret, fn_body) = match func.deref().deref() {
            TreeNode::FunctionDecl {
                name,
                args,
                ret,
                body,
            } => (name, args, ret, body),
            _ => unreachable!(),
        };

        // Make sure the name of the function is not already used
        let compiled_func = self.module.get_function(&fn_name);
        if compiled_func.is_some() {
            return Err(self.get_trace(CodegenError::NameAlreadyUsed, func));
        }

        // Compile function prototype
        let fn_proto = match self.compile_proto(&fn_name, &fn_args, &fn_ret) {
            Ok(proto) => proto,
            Err(err) => return Err(self.get_trace(err, func)),
        };

        // Prepare the body of the function
        let entry = self.context.append_basic_block(fn_proto, "entry");
        self.builder.position_at_end(entry);

        // Build each argument of the function
        for (i, arg) in fn_proto.get_param_iter().enumerate() {
            let arg_name = &fn_args[i].1;

            // Create pointer
            let ptr = self.builder.build_alloca(arg.get_type(), arg_name);
            // Store argument in the pointer
            self.builder.build_store(ptr, arg);
            // Save variable
            self.variables.insert(arg_name.clone(), ptr);
        }

        // Compile each node (line) of the function
        for node in fn_body {
            unwrap_or_return!(self.compile_node(&fn_proto, &node));
        }

        // Return void if no return instruction was found
        if entry.get_terminator().is_none() {
            self.builder.build_return(None);
        }

        if fn_proto.verify(false) {
            // Run function optimization
            self.fpm.run_on(&fn_proto);
            return Ok(fn_proto);
        }

        // Get error string
        Err(self
            .get_trace(CodegenError::ModuleError(self.module_error()), func))
    }

    /// Compiles a function prototype. A prototype is everything of a function
    /// except its body.
    pub fn compile_proto(
        &self,
        fn_name: &String,
        fn_args: &Vec<(TypeKind, String)>,
        fn_ret: &TypeKind,
    ) -> Result<FunctionValue<'ctx>, CodegenError> {
        // Contains type of each argument
        let mut args_types: Vec<BasicTypeEnum> =
            Vec::with_capacity(fn_args.len());

        // Get each argument type
        for arg in fn_args {
            // Identify type
            let arg_type = unwrap_or_return!(self.get_type(&arg.0));
            args_types.push(arg_type.unwrap());
        }

        // Function return type
        let fn_type = unwrap_or_return!(self.get_type(fn_ret));

        // Apply function type
        let fn_type = match fn_type {
            Some(ty) => ty.fn_type(&args_types, false),
            None => self.context.void_type().fn_type(&args_types, false),
        };

        // Add the function to the module
        let func = self.module.add_function(&fn_name, fn_type, None);
        // Set the name of each argument of the function
        for (i, arg) in func.get_param_iter().enumerate() {
            arg.set_name(&fn_args[i].1);
        }

        Ok(func)
    }

    /// Compiles a node in a given `parent` function
    pub fn compile_node(
        &mut self,
        parent: &FunctionValue,
        node: &TraceInfo<Box<TreeNode>>,
    ) -> CodegenResult<InstructionValue<'ctx>> {
        match node.deref().deref() {
            TreeNode::Return(expr) => {
                if expr.is_none() {
                    return Ok(self.builder.build_return(None));
                }

                // Compile return value
                let compiled_expr = unwrap_or_return!(
                    self.compile_expr(expr.as_ref().unwrap())
                );
                let ret = self.builder.build_return(Some(&compiled_expr));
                check_error!(self, node);

                Ok(ret)
            }
            TreeNode::Condition {
                cond,
                then_body,
                else_body,
            } => {
                let compiled_cond = unwrap_or_return!(self.compile_expr(cond));
                let then_bb = self.context.append_basic_block(*parent, "then");
                let else_bb = self.context.append_basic_block(*parent, "else");

                let branch = self.builder.build_conditional_branch(
                    compiled_cond.into_int_value(),
                    then_bb,
                    else_bb,
                );

                self.builder.position_at_end(then_bb);
                for node in then_body {
                    unwrap_or_return!(self.compile_node(parent, &node));
                }

                self.builder.position_at_end(else_bb);
                for node in else_body {
                    unwrap_or_return!(self.compile_node(parent, &node));
                }

                Ok(branch)
            }
            TreeNode::FunctionCall { name, args } => {
                match self.module.get_function(&name) {
                    Some(func) => {
                        let mut compiled_args = Vec::with_capacity(args.len());

                        // Compile each argument of the function call
                        for arg in args {
                            let expr =
                                unwrap_or_return!(self.compile_expr(&arg));
                            compiled_args.push(expr);
                        }

                        // Build instruction
                        let build_call = self
                            .builder
                            .build_call(func, &compiled_args, "tmp_call")
                            .try_as_basic_value();

                        // If the call couldn't be built as an instruction, this
                        // means that the function returns a non-void type, and
                        // thus, we must convert the call to an instruction
                        // value
                        if build_call.is_left() {
                            return Ok(build_call
                                .left()
                                .unwrap()
                                .as_instruction_value()
                                .unwrap());
                        }

                        Ok(build_call.right().unwrap())
                    }
                    None => {
                        Err(self
                            .get_trace(CodegenError::FunctionNotFound, node))
                    }
                }
            }
            TreeNode::VariableDecl { name, value } => {
                let value = unwrap_or_return!(self.compile_expr(value));

                // Create pointer
                let ptr = self.builder.build_alloca(value.get_type(), name);
                // Store variable in the pointer
                self.builder.build_store(ptr, value);
                // Save variable
                self.variables.insert(name.clone(), ptr);

                Ok(self.builder.build_store(ptr, value))
            }
            _ => todo!(),
        }
    }

    /// Compiles an expression
    pub fn compile_expr(
        &self,
        expr: &TraceInfo<Box<TreeNode>>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr.deref().deref() {
            TreeNode::BinaryOp {
                operator,
                left,
                right,
            } => {
                let left_hand = unwrap_or_return!(self.compile_expr(&left));
                let right_hand = unwrap_or_return!(self.compile_expr(&right));

                // We only support integers operations for now
                if !left_hand.get_type().is_int_type()
                    || !right_hand.get_type().is_int_type()
                {
                    return Err(
                        self.get_trace(CodegenError::InvalidOperation, expr)
                    );
                }

                // Cast to integers
                let left_hand = left_hand.into_int_value();
                let right_hand = right_hand.into_int_value();

                // Build instruction
                match operator {
                    OperatorKind::Add => Ok(self
                        .builder
                        .build_int_add(left_hand, right_hand, "add")
                        .as_basic_value_enum()),
                    OperatorKind::Sub => Ok(self
                        .builder
                        .build_int_sub(left_hand, right_hand, "sub")
                        .as_basic_value_enum()),
                    OperatorKind::Mul => Ok(self
                        .builder
                        .build_int_mul(left_hand, right_hand, "mul")
                        .as_basic_value_enum()),
                    OperatorKind::Div => Ok(self
                        .builder
                        .build_int_signed_div(left_hand, right_hand, "div")
                        .as_basic_value_enum()),
                    OperatorKind::Mod => Ok(self
                        .builder
                        .build_int_signed_rem(left_hand, right_hand, "rem")
                        .as_basic_value_enum()),
                    OperatorKind::Eq => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            left_hand,
                            right_hand,
                            "cmp",
                        )
                        .as_basic_value_enum()),
                    _ => todo!(),
                }
            }
            TreeNode::Integer(int) => Ok(self
                .context
                .i64_type()
                .const_int(*int as u64, false)
                .as_basic_value_enum()),
            TreeNode::Boolean(bool) => Ok(self
                .context
                .bool_type()
                .const_int(*bool as u64, false)
                .as_basic_value_enum()),
            TreeNode::VarCall(name) => match self.variables.get(name) {
                Some(var) => Ok(self.builder.build_load(*var, name)),
                None => {
                    Err(self.get_trace(CodegenError::VariableNotFound, expr))
                }
            },
            TreeNode::FunctionCall { name, args } => {
                match self.module.get_function(&name) {
                    Some(func) => {
                        let mut compiled_args = Vec::with_capacity(args.len());

                        // Compile each argument of the function call
                        for arg in args {
                            let expr =
                                unwrap_or_return!(self.compile_expr(&arg));
                            compiled_args.push(expr);
                        }

                        // Build instruction
                        let build_call = self
                            .builder
                            .build_call(func, &compiled_args, "tmp_call")
                            .try_as_basic_value();

                        let instr = build_call.left();
                        if instr.is_none() {
                            return Err(self
                                .get_trace(CodegenError::VoidFunction, expr));
                        }

                        Ok(instr.unwrap())
                    }
                    None => {
                        Err(self
                            .get_trace(CodegenError::FunctionNotFound, expr))
                    }
                }
            }
            _ => todo!(),
        }
    }

    /// Compile LLVM IR to binary file
    pub fn write_to_file(&self, obj_path: &str, bin_path: &str) {
        // Write file object
        self.target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(obj_path))
            .expect("Couldn't write object to file.");

        // Link object
        Command::new("gcc")
            .args(&[obj_path, "-o", bin_path])
            .output()
            .expect("Error when linking.");

        // Execute program
        let prog_output = Command::new(bin_path)
            .output()
            .expect("Couldn't run the program.");

        // Print program's output
        println!("{}", std::str::from_utf8(&prog_output.stdout).unwrap());
    }
}