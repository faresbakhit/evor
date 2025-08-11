use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use evorc::analyzer::Analyzer;
use evorc::lexer::Lexer;
use evorc::parser::Parser;
use std::env;
use std::fs;
use std::io::{self, Read};
use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let mut input = String::new();
    let filename = if args.len() > 1 {
        input = fs::read_to_string(&args[1]).expect("Failed to read file");
        args[1].as_str()
    } else {
        io::stdin()
            .read_to_string(&mut input)
            .expect("Failed to read stdin");
        "<stdin>"
    };
    let mut parser = Parser::new(Lexer::new(&input));
    match match parser.parse() {
        Ok(prog) => {
            // println!("{prog:#?}");
            let mut analyzer = Analyzer::new(parser);
            match analyzer.analyze(prog) {
                Ok(prog) => {
                    println!("{prog:#?}");
                    Ok(())
                }
                Err(err) => Err((err.span, "bad semantics", err.message)),
            }
        }
        Err(err) => Err((
            err.span,
            "bad syntax",
            format!("expected {}, found {}", err.expected, err.found),
        )),
    } {
        Ok(()) => ExitCode::SUCCESS,
        Err((span, category, message)) => {
            let mut files = SimpleFiles::new();
            let span = span.as_range();
            let file_id = files.add(filename, input);
            let diagnostic = Diagnostic::error()
                .with_message(category)
                .with_label(Label::primary(file_id, span).with_message(message));
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            ExitCode::FAILURE
        }
    }
}
