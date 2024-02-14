use std::{
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

use ize::{
    ast::{Command, CommandKind, ExpressionKind},
    grammar_cmd, lexer,
    pos::RangePos,
};

fn main() {}

/* TODO: IMPORT MODULES
1. Convert import path into an actual file path.
2. Read file, parse the code and generate a Import Symbol Table (IST) containing the symbols requested in the import, or all if "*".
3. Link the IST to the base AST (the code that called the import command).

We need a struct that contains all ther ASTs, symbol tables and links from symbol tables to ASTs.
*/

// ------ DOES NOT COMPILE ------
/*

fn main() {
    let file_path = "izeware/experiment_semcheck.iz";

    parse_and_import(file_path);

    // println!("\n========== SEMCHECKER ==========\n\n");

    // let symtab = semcheck::check_ast(&ast).expect("Error semchecking the AST");
    // println!("Symbol table = {:#?}", symtab);

    //TODO: transpile
}

fn parse_and_import(file_path: &str) {
    let ast = parse_file(file_path);

    let mut commands = vec![];
    let mut imports = vec![];
    for cmd in ast {
        if let CommandKind::Import(_) = &cmd.kind {
            imports.push(cmd);
        } else {
            commands.push(cmd);
        }
    }

    //TODO: last component of path might be a symbol name, not a file name

    let import_paths: Vec<_> = imports
        .into_iter()
        .map(|cmd| into_file_paths(cmd))
        .flatten()
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
    pub path: String,
    pub alias: Option<(String, RangePos)>,
}

impl Import {
    pub fn new(path: String, alias: Option<(String, RangePos)>, pos: RangePos) -> Self {
        Self { pos, path, alias }
    }
}

fn into_file_paths(cmd: Command) -> Vec<Import> {
    if let CommandKind::Import(expr_vec) = cmd.kind {
        let path_vec: Vec<Import> = expr_vec
            .into_iter()
            .map(|expr| {
                if let ExpressionKind::Path { module_path, alias } = expr.kind {
                    let path = module_path.into_iter().fold("".to_owned(), |acc, x| {
                        if acc.is_empty() {
                            x.id
                        } else {
                            acc + "/" + &x.id
                        }
                    });
                    let alias = if let Some(ident) = alias {
                        Some((ident.id, ident.pos))
                    } else {
                        None
                    };
                    Import::new(path, alias, cmd.pos)
                } else {
                    panic!("Not a path expression")
                }
            })
            .collect();
        path_vec
    } else {
        panic!("Not an import command")
    }
}

*/