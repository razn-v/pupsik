use crossterm::execute;
use crossterm::style::{
    Attribute, Color, Print, ResetColor, SetAttribute, SetForegroundColor,
};

use std::ops::Deref;

#[macro_export]
macro_rules! report_error {
    ($manager:ident, $err:expr) => {
        $manager.report_error($err);
        std::process::exit(1);
    };
}

/// Struct used to report error
#[derive(Debug, Clone)]
pub struct TraceInfo<T> {
    pub value: T,
    /// Line number
    pub n_line: usize,
    /// Position in line
    pub pos: usize,
    /// Number of characters
    pub len: usize,
}

impl<T> TraceInfo<T> {
    pub fn new(value: T, n_line: usize, pos: usize, len: usize) -> Self {
        Self {
            value,
            n_line,
            pos,
            len,
        }
    }
}

impl<T> Deref for TraceInfo<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

/// Struct used to report errors. A single report will cause the program to
/// print the line where the issue is and to exit.
pub struct ErrorManager<'a> {
    /// Name of the target file
    pub filename: &'a str,
    /// Lines of the file
    pub lines: &'a Vec<String>,
}

impl<'a> ErrorManager<'a> {
    pub fn new(filename: &'a str, lines: &'a Vec<String>) -> Self {
        Self { filename, lines }
    }

    /// Takes a compile `error` and prints it in pretty way
    pub fn report_error<T: CompileError>(&self, error: TraceInfo<T>) {
        let mut error = error;
        // Space before vertical bar
        let padding = 2 * error.n_line.to_string().len() + 1;
        // The line where the error occured
        let mut line = self.lines.get(error.n_line).unwrap().clone();

        // Truncate error length if overflowing
        if error.len > line.len() {
            line.push_str(" ...");
            error.len = line.len();
        }

        // Calculate the number of spaces in the slice of code to avoid wrong
        // formatting
        let n_spaces = match &line.get(error.pos..=error.pos + error.len) {
            Some(code) => code.matches(' ').count(),
            None => 0,
        };

        // Fix the number of arrows
        error.len += n_spaces;

        // Print the error
        execute!(
            std::io::stderr(),
            SetForegroundColor(Color::Red),
            Print("[error]"),
            ResetColor,
            Print(format!(
                " {}:{}:{}\n",
                self.filename,
                error.n_line + 1,
                error.pos + 1
            )),
            SetForegroundColor(Color::Grey),
            SetAttribute(Attribute::Bold),
            Print(format!("{:>padding$}\n", "|", padding = padding)),
            Print(format!("{} | ", error.n_line + 1)),
            ResetColor,
            Print(format!("{}\n", line)),
            SetForegroundColor(Color::Grey),
            SetAttribute(Attribute::Bold),
            Print(format!("{:>padding$} ", "|", padding = padding)),
            ResetColor,
            SetForegroundColor(Color::Red),
            Print(format!(
                "{:>arrow_pos$} {}\n",
                "^".repeat(error.len),
                error.error_msg(),
                arrow_pos = error.pos + error.len
            )),
            ResetColor,
        )
        .unwrap()
    }
}

pub trait CompileError {
    fn error_msg(&self) -> String;
}
