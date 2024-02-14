use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

use ize::{
    ast::{Command, CommandKind, ExpressionKind, Identifier, Primary},
    grammar_cmd, lexer,
    pos::RangePos,
};

/* TODO: IMPORT MODULES
1. Convert import path into an actual file path.
2. Read file, parse the code and generate a Import Symbol Table (IST) containing the symbols requested in the import, or all if "*".
3. Link the IST to the base AST (the code that called the import command).

We need a struct that contains all ther ASTs, symbol tables and links from symbol tables to ASTs.
*/

fn main() {
    let file_path = "izeware/experiment_semcheck.iz";

    parse_and_import(file_path);

    // println!("\n========== SEMCHECKER ==========\n\n");

    // let symtab = semcheck::check_ast(&ast).expect("Error semchecking the AST");
    // println!("Symbol table = {:#?}", symtab);

    //TODO: transpile
}

// Parse file, then generate imports and parse imported files too.
fn parse_and_import(file_path: &str) {
    let ast = parse_file(file_path);

    let mut commands = vec![];
    let mut imports = vec![];
    for cmd in ast {
        if let CommandKind::Import { .. } = &cmd.kind {
            imports.push(cmd);
        } else {
            commands.push(cmd);
        }
    }

    let import_paths: Vec<_> = imports
        .into_iter()
        .map(|cmd| into_file_paths(cmd))
        .collect();

    println!("\n========== IMPORTS ({file_path}) ==========\n\n");

    println!("{:#?}", import_paths);

    for path in import_paths {
        let file_path = path.path + ".iz";
        parse_and_import(file_path.as_str());
    }
}

fn parse_file(file_path: &str) -> Vec<Command> {
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let code = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    let tokens = lexer::tokenize(code).expect("Bad token");

    let mut input = tokens.as_slice();

    println!("\n========== PARSER ({file_path}) ==========\n\n");

    let mut ast = vec![];

    while !input.is_empty() {
        let (rest, parsed, _) = grammar_cmd::cmd(input).expect("Error parsing command");
        let parsed = parsed.cmd().expect("Node must be a command");
        println!("-------------------------\n{:#?}", parsed);
        input = rest;
        ast.push(parsed);
    }

    ast
}

#[derive(Debug)]
pub struct Import {
    pub pos: RangePos,
    pub imports: Vec<(Identifier, Option<Identifier>)>,
    pub path: String,
}

impl Import {
    pub fn new(imports: Vec<(Identifier, Option<Identifier>)>, path: String, pos: RangePos) -> Self {
        Self { pos, imports, path }
    }
}

fn into_file_paths(cmd: Command) -> Import {
    if let CommandKind::Import { imports, path } = cmd.kind {
        if let ExpressionKind::Dot(dot_path_vec) = path.kind {
            let path = dot_path_vec.into_iter().map(|expr| {
                if let ExpressionKind::Primary(Primary::Identifier(id)) = expr.kind {
                    id
                } else {
                    panic!("Not a primary identifier")
                }
            }).fold("".to_owned(), |acc, x| {
                if acc.is_empty() {
                    x
                } else {
                    acc + "/" + &x
                }
            });
            Import::new(imports, path, cmd.pos)
        } else {
            panic!("Not a dot expression")
        }
    } else {
        panic!("Not an import command")
    }
}
