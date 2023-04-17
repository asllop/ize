use ize::lexer::Line;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    println!("--- IZE Lexer:\n");

    let file = File::open("Cargo.toml")?;
    let reader = BufReader::new(file);

    // let mut lines = vec![];
    // for line in reader.lines() {
    //     let line = line?;
    //     match line.as_str().scan_tokens() {
    //         Ok(mut line) => { lines.append(&mut line) },
    //         Err(_err) => todo!("Error handling"),
    //     }
    // }

    let lines: Vec<Line> = reader.lines().enumerate().map(|(_line_num, line)| {
        let line = line.expect("Error reading line of code from file");
        match line.as_str().try_into() {
            Ok(line) => line,
            Err(_err) => todo!("Error handling"),
        }
    }).collect();

    println!("Num Lines = {}", lines.len());

    Ok(())
}
