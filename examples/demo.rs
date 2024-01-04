use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str::{self},
};

use ize::{grammar, lexer};

fn main() {
    let file_path = "izeware/experiment.iz";
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let code = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    let tokens = lexer::tokenize(code).expect("Bad token");

    let mut input = tokens.as_slice();

    while !input.is_empty() {
        let (rest, matched, _) = grammar::expr(input).expect("Error parsing expr");
        println!("-------------------------\n{:#?}", matched);
        input = rest;
    }
}
