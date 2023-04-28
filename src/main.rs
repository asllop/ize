use ize::lexer::Line;
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
                        "Error: \"{}\" at line {} offset {}",
                        err.message,
                        line_num + 1,
                        err.position + 1
                    );
                    exit(1);
                }
            }
        })
        .collect();

    // Print tokens
    for l in lines {
        for _ in 0..l.position.indentation {
            print!(" ");
        }
        for t in l.tokens {
            print!("{:?}({:?}) ", t.token_type, t.data);
        }
        println!("EOL");
    }
    println!("\nEOF");

    Ok(())
}
