use ize::{
    lexer::{Line, TokenData},
    parser::LineParser,
};
use std::{
    fs::File,
    io::{self, prelude::*, BufReader},
    process::exit,
};

fn main() -> io::Result<()> {
    let file = File::open("izeware/test.iz")?;
    let reader = BufReader::new(file);

    let lines: Vec<Line> = reader
        .lines()
        .enumerate()
        .map(|(line_num, line)| {
            let line = line.expect("Error reading line of code from file");
            match Line::scan_tokens(&line, line_num) {
                Ok(line) => line,
                Err(err) => {
                    println!(
                        "Lexer Error: \"{}\" at line {} offset {}",
                        err.message,
                        line_num + 1,
                        err.position + 1
                    );
                    exit(1);
                }
            }
        })
        .collect();

    println!("------- LEXER\n");

    // Print tokens
    for l in &lines {
        for _ in 0..l.position.indentation {
            print!(" ");
        }
        for t in &l.tokens {
            if let TokenData::None = t.data {
                print!("{} ", t.token_type);
            } else {
                print!("{}{} ", t.token_type, t.data);
            }
        }
        println!("");
    }
    println!("");

    println!("------- PARSER\n");

    for l in lines {
        let line_num = l.position.line_num;
        match LineParser::parse(l) {
            Ok(expr) => println!("EXPR => {}", expr),
            Err(err) => {
                println!(
                    "Parser Error: \"{}\" at line {} offset {}",
                    err.message,
                    line_num + 1,
                    err.offset + 1
                );
                exit(2);
            }
        }
    }
    println!("");

    Ok(())
}
