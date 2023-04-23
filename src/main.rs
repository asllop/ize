use ize::lexer::Line;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    println!("--- IZE Lexer:\n");

    let file = File::open("test.iz")?;
    let reader = BufReader::new(file);

    let lines: Vec<Line> = reader.lines().enumerate().map(|(line_num, line)| {
        let line_str = line.expect("Error reading line of code from file");

        let r = match Line::scan_tokens(&line_str, line_num) {
            Ok(line) => {
                println!("Code: \'{}\'", line_str.trim());
                println!("Line num: {}", line.position.line_num);
                println!("Indentation num: {}", line.position.indentation);
                println!("Indentation Type: {:?}", line.position.indentation_type);
                println!("------------------------------------------");
                line
            },
            Err(_err) => todo!("Error handling"),
        };
        r
    }).collect();

    println!("Num Lines = {}", lines.len());
    
    Ok(())
}
