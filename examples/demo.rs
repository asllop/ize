use ize::ast::ImportPath;
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

fn main() {
    let code = read_code("izeware/test_mod.iz");
    let ast = ize::build(code.as_str(), pkg_reader).expect("Error parsing program");

    println!("{:#?}", ast);
}

fn pkg_reader<'a>(import_path: &ImportPath) -> String {
    match import_path {
        ImportPath::Dot(_dot_path) => todo!("Implement dot path"),
        ImportPath::File(file_path) => read_code(file_path.as_str()),
    }
}

fn read_code(path: &str) -> String {
    let file = File::open(path).expect(&format!("Error opening file {}", path));
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader
        .read_to_end(&mut buf)
        .expect("Failed reading file into buffer");
    let code = match str::from_utf8(&buf) {
        Ok(v) => v,
        Err(e) => panic!("Invalid UTF-8 string: {}", e),
    };
    code.into()
}

// fn main() {
//     let file = File::open("izeware/test_stmt.iz").expect("Error opening file");
//     let mut reader = BufReader::new(file);
//     let mut buf = Vec::<u8>::new();
//     reader
//         .read_to_end(&mut buf)
//         .expect("Failed reading file into buffer");
//     let code = match str::from_utf8(&buf) {
//         Ok(v) => v,
//         Err(e) => panic!("Invalid UTF-8 string: {}", e),
//     };

//     println!("\n------ LEXER\n");

//     let mut tokens = Vec::new();
//     let mut lexer = Lexer::new(code);
//     loop {
//         let token = lexer.scan_token().expect("Error scanning tokens");
//         match token.lexeme {
//             Lexeme::EOF => break,
//             Lexeme::Nothing => {}
//             _ => {
//                 println!("{:?}", token);
//                 tokens.push(token);
//             }
//         }
//     }

//     println!("\n------ PARSER\n");

//     let mut parser = Parser::new(tokens);
//     let commands = parser.parse().expect("Error parsing");
//     commands.iter().for_each(|cmd| {
//         println!("-------------------\n{:#?}", cmd);
//     });
// }
