use ize::{
    ast::{DotPath, ImportPath},
    lexer::Lexer,
    parser::grammar,
    BuildErr, IzeErr, Pos,
};
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str::{self},
};

fn main() {
    let file_path = "izeware/test_grammar2.iz";
    let code = read_code(file_path).expect("Error reading file");
    let mut token_stream = Lexer::new(code.as_str())
        .tokenize()
        .expect("Error tokenizing");
    // while !token_stream.ended() {
    //     let expr = grammar::parse_expression(&mut token_stream).expect("Error parsing expr");
    //     println!("{:#?}", expr);
    //     println!("-----------------------");
    // }
    println!("------ CHECK ------");

    let mut index = 0;
    loop {
        let (result, new_index) =
            grammar::check_expression(&mut token_stream, index).expect("Error checking expression");
        if !result {
            println!("\nFAIL");
            break;
        }
        if new_index == token_stream.len() {
            println!("\nEND OF TOKEN STREAM");
            break;
        } else {
            index = new_index;
        }
    }

    println!("------ PARSE ------");
    grammar::parse_expression(&mut token_stream).expect("Error parsing expr 1");
    println!("------------");
    grammar::parse_expression(&mut token_stream).expect("Error parsing expr 2");
    println!("------------");
    grammar::parse_expression(&mut token_stream).expect("Error parsing expr 3");
    println!("------------");
}

fn _main() {
    let file_path = "izeware/test_mod.iz";
    let code = read_code(file_path).expect("Error reading file");
    let ast = ize::build(
        code.as_str(),
        ImportPath::File(file_path.into()),
        pkg_reader,
    )
    .expect("Error parsing program");

    println!("{:#?}", ast);
}

fn pkg_reader<'a>(import_path: &ImportPath, import_pos: Pos) -> Result<String, IzeErr> {
    let file_path = match import_path {
        ImportPath::Dot(dot_path) => convert_dot_path(dot_path),
        ImportPath::File(file_path) => file_path.clone(),
    };

    let code = read_code(file_path.as_str()).or_else(|e| match e {
        ReadCodeErr::IoErr(e) => {
            Result::ize_err(format!("IO error on file {}: {}", file_path, e), import_pos)
        }
        ReadCodeErr::Utf8Err(e) => Result::ize_err(
            format!("UTF-8 conversion error on file {}: {}", file_path, e),
            import_pos,
        ),
    })?;
    Ok(code)
}

fn convert_dot_path(dot_path: &DotPath) -> String {
    "izeware/".to_owned() + &dot_path.path.join("/") + ".iz"
}

#[derive(Debug)]
enum ReadCodeErr {
    IoErr(std::io::Error),
    Utf8Err(std::str::Utf8Error),
}

fn read_code(path: &str) -> Result<String, ReadCodeErr> {
    let file = File::open(path).or_else(|e| Err(ReadCodeErr::IoErr(e)))?;
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader
        .read_to_end(&mut buf)
        .or_else(|e| Err(ReadCodeErr::IoErr(e)))?;
    let code = str::from_utf8(&buf).or_else(|e| Err(ReadCodeErr::Utf8Err(e)))?;
    Ok(code.into())
}
