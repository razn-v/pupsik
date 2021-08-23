mod lexer;
mod token;

use std::io::BufRead;

use lexer::Lexer;

fn main() {
    let stdin = std::io::stdin();

    // Read input line by line
    for line in stdin.lock().lines() {
        let line = line.expect("Couldn't read input.");

        for token in Lexer::new(line) {
            println!("{:?}", token);
        }
    }
}
