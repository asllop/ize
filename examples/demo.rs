use ize::{ast::ImportPath, BuildErr, IzeErr, Pos};
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str::{self},
};

fn main() {
    let code = read_code("izeware/test_mod.iz").expect("Error reading file");
    let ast = ize::build(code.as_str(), pkg_reader).expect("Error parsing program");

    println!("{:#?}", ast);
}

fn pkg_reader<'a>(import_path: &ImportPath, import_pos: Pos) -> Result<String, IzeErr> {
    match import_path {
        ImportPath::Dot(_dot_path) => todo!("Implement dot path"),
        ImportPath::File(file_path) => {
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
    }
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
