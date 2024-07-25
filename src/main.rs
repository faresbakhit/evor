use std::{
    env,
    fs::File,
    io::{self, stdin, Read},
    process::ExitCode,
};

use machine::lexer::Lexer;

fn read_tokens(mut file: impl Read) -> io::Result<()> {
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    for token in Lexer::new(buf.as_str()) {
        println!(
            "{:?} [{:?}) ({:?})",
            token.kind,
            token.span.as_range(),
            &buf[token.span]
        );
    }
    Ok(())
}

fn main() -> ExitCode {
    let res = match env::args_os().nth(1) {
        Some(path) if path.as_os_str() == "-" => read_tokens(stdin()),
        Some(path) => match File::open(path) {
            Ok(file) => read_tokens(file),
            Err(err) => {
                eprintln!("error: {}", err);
                return ExitCode::FAILURE;
            }
        },
        None => read_tokens(stdin()),
    };
    res.unwrap();
    ExitCode::SUCCESS
}
