mod lexer;
mod node;
mod parser;
mod token;

use std::io::BufRead;

use crate::parser::Parser;
use lexer::Lexer;

fn main() {
    let stdin = std::io::stdin();

    // Read input line by line
    for line in stdin.lock().lines() {
        let line = line.expect("Couldn't read input.");

        // Collect lexer output
        let tokens = Lexer::new(line).into_iter().map(|x| x.unwrap()).collect();

        // Call parser with lexer output and iterate over the result
        for func in Parser::new(tokens) {
            println!("{:?}", func);
        }
    }
}
