use ize::lexer::Line;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    println!("--- IZE Lexer:\n");

    let file = File::open("test.iz")?;
    let reader = BufReader::new(file);

    let lines: Vec<Line> = reader.lines().enumerate().map(|(line_num, line)| {
        let line_str = line.expect("Error reading line of code from file");

        let line = match Line::scan_tokens(&line_str, line_num) {
            Ok(line) => {
                println!("Code: \'{}\'", line_str.trim());
                println!("Line num: {}", line.position.line_num);
                println!("Indentation num: {}", line.position.indentation);
                println!("Indentation Type: {:?}", line.position.indentation_type);
                println!("------------------------------------------");
                line
            },
            Err(err) => panic!("Error: {}", err.message),
        };
        line
    }).collect();

    println!("Num Lines = {}", lines.len());

    for l in lines {
        for t in l.tokens {
            print!("{:?} , ", t.id);
        }
    }

    println!();
    
    Ok(())
}
