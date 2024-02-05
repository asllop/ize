use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str::{self},
};

use ize::{grammar_cmd, lexer, semcheck};

fn main() {
    let file_path = "izeware/experiment_semcheck.iz";
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let code = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    let tokens = lexer::tokenize(code).expect("Bad token");

    let mut input = tokens.as_slice();

    println!("\n========== PARSER ==========\n\n");

    let mut ast = vec![];
    while !input.is_empty() {
        let (rest, parsed, _) = grammar_cmd::cmd(input).expect("Error parsing command");
        println!("-------------------------\n{:#?}", parsed);
        input = rest;
        ast.push(parsed);
    }

    println!("\n========== SEMCHECKER ==========\n\n");

    let symtab = semcheck::check_ast(&ast).expect("Error semchecking the AST");
    println!("Symbol table = {:#?}", symtab);

    //TODO: transpile
}
