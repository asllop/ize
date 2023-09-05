use ize::{
    lexer::{Lexeme, Lexer},
    parser::Parser,
};
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

fn main() {
    let file = File::open("izeware/test_expr.iz").expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader
        .read_to_end(&mut buf)
        .expect("Failed reading file into buffer");
    let code = match str::from_utf8(&buf) {
        Ok(v) => v,
        Err(e) => panic!("Invalid UTF-8 string: {}", e),
    };

    println!("\n------ LEXER\n");

    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(code);
    loop {
        let token = lexer.scan_token().expect("Error scanning tokens");
        match token.lexeme {
            Lexeme::EOF => break,
            Lexeme::Nothing => {}
            _ => {
                println!("{:?}", token);
                tokens.push(token);
            }
        }
    }

    println!("\n------ PARSER\n");

    let mut parser = Parser::new(tokens);
    //let _commands = parser.parse().expect("Error parsing");
    while !parser.ended() {
        let expr = parser.expression().expect("Error parsing expression");
        println!("-------------------\n{:#?}", expr);
    }
}
