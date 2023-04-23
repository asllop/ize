use ize::lexer::Line;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    println!("--- IZE Lexer:\n");

    let file = File::open("Cargo.toml")?;
    let reader = BufReader::new(file);

    let lines: Vec<Line> = reader.lines().enumerate().map(|(_line_num, line)| {
        let line = line.expect("Error reading line of code from file");
        match line.parse() {
            Ok(line) => line,
            Err(_err) => todo!("Error handling"),
        }
    }).collect();

    println!("Num Lines = {}", lines.len());

    Ok(())
}
