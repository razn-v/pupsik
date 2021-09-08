mod codegen;
mod error;
mod lexer;
mod node;
mod parser;
mod token;

use crate::codegen::Codegen;
use crate::error::ErrorManager;
use crate::error::TraceInfo;
use crate::parser::Parser;
use crate::token::Token;

use inkwell::context::Context;
use lexer::Lexer;

use std::fs::File;
use std::io::BufRead;

/// Returns the contained [`Ok`] value or returns error contained in [`Err`]
#[macro_export]
macro_rules! unwrap_or_return {
    ($item:expr) => {
        match $item {
            Ok(x) => x,
            Err(err) => return Err(err),
        }
    };
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // If file is not specified
    if args.len() == 1 {
        println!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    // Read file lines
    let filename = &args[1];
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(_) => {
            report_error!("Couldn't open file.");
        }
    };
    let lines: Vec<String> = std::io::BufReader::new(file)
        .lines()
        .map(|line| line.expect("Couldn't read line."))
        .collect();

    let err_manager = ErrorManager::new(filename, &lines);
    let context = Context::create();
    let mut codegen = Codegen::new(&context, "main_mod");

    // Lex each line and save tokens
    let mut tokens: Vec<TraceInfo<Token>> = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        // Trace each character
        let traced_line: Vec<TraceInfo<char>> = line
            .chars()
            .enumerate()
            .map(|(j, chr)| TraceInfo::new(chr, i, j, 1))
            .collect();

        for token in Lexer::new(&traced_line) {
            if token.is_err() {
                report_error!(err_manager, token.unwrap_err());
            }
            tokens.push(token.unwrap());
        }
    }

    // Parse tokens into top-level instructions
    for toplevel in Parser::new(&tokens) {
        if toplevel.is_err() {
            report_error!(err_manager, toplevel.unwrap_err());
        }

        // Compile each function (for now we only support functions)
        if let Err(err) = codegen.compile_func(&toplevel.unwrap()) {
            report_error!(err_manager, err);
        }
    }

    // Compile LLVM IR to binary file
    if cfg!(windows) {
        codegen.write_to_file("./prog.obj", "./prog.exe");    
    } else {
        codegen.write_to_file("./prog.o", "./prog");
    }
}
