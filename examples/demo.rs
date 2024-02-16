use std::{
    env,
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

use ize::{
    ast::{Command, CommandKind, ExpressionKind, ImportSymbol, Primary},
    grammar_cmd, lexer,
    pos::RangePos,
};

/* TODO: IMPORT MODULES
1. Convert import path into an actual file path.
2. Read file, parse the code and generate a Import Symbol Table (IST) containing the symbols requested in the import, or all if "*".
3. Link the IST to the base AST (the code that called the import command).

We need a struct that contains all ther ASTs, symbol tables and links from symbol tables to ASTs.
*/

#[derive(Debug)]
pub struct Ast {
    /// Source code file path.
    pub file_path: String,
    /// Parsed commands, without imports.
    pub commands: Vec<Command>,
    /// Imported modules and required symbols.
    pub imports: Vec<(Ast, Vec<ImportSymbol>)>,
    //TODO: Symbol table.
}

fn main() {
    let file_path = "izeware/experiment_semcheck.iz";
    parse_and_import(absolute_path(file_path).as_str());

    // println!("\n========== SEMCHECKER ==========\n\n");

    // let symtab = semcheck::check_ast(&ast).expect("Error semchecking the AST");
    // println!("Symbol table = {:#?}", symtab);

    //TODO: transpile
}

fn absolute_path(path_str: &str) -> String {
    let mut path_buf = env::current_dir().expect("Error gettingcurrent dir");
    path_buf.push(path_str);
    path_buf.to_string_lossy().into()
}

// Parse file, then generate imports and parse imported files too.
fn parse_and_import(file_path: &str) {
    let ast = parse_file(file_path);

    // Segregate imports from other commands
    let (imports, commands): (Vec<_>, Vec<_>) = ast.into_iter().partition(|cmd| {
        if let CommandKind::Import { .. } = &cmd.kind {
            true
        } else {
            false
        }
    });

    // Convert import commands into import paths.
    let import_path_vec: Vec<ImportPaths> = imports.into_iter().map(|cmd| cmd.into()).collect();

    println!("\n========== COMMANDS ({file_path}) ==========\n\n");

    for cmd in &commands {
        println!("----------------> {:#?}\n", cmd);
    }
    
    println!("\n========== IMPORTS ({file_path}) ==========\n\n");

    println!("{:#?}", import_path_vec);

    // Parse imported files
    for import_path in &import_path_vec {
        let file_path = import_path.path.clone() + ".iz";
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

    let mut ast = vec![];

    while !input.is_empty() {
        let (rest, parsed, _) = grammar_cmd::cmd(input).expect("Error parsing command");
        let parsed = parsed.cmd().expect("Node must be a command");
        input = rest;
        ast.push(parsed);
    }

    ast
}

#[derive(Debug)]
pub struct ImportPaths {
    pub pos: RangePos,
    pub symbols: Vec<ImportSymbol>,
    pub path: String,
}

impl ImportPaths {
    pub fn new(symbols: Vec<ImportSymbol>, path: String, pos: RangePos) -> Self {
        Self { pos, symbols, path }
    }
}

impl From<Command> for ImportPaths {
    fn from(cmd: Command) -> Self {
        if let CommandKind::Import { symbols, path } = cmd.kind {
            if let ExpressionKind::Dot(dot_path_vec) = path.kind {
                let path = dot_path_vec
                    .into_iter()
                    .map(|expr| {
                        if let ExpressionKind::Primary(Primary::Identifier(id)) = expr.kind {
                            id
                        } else {
                            panic!("Not a primary identifier")
                        }
                    })
                    .reduce(|acc, x| acc + "/" + &x)
                    .unwrap();
                Self::new(symbols, absolute_path(path.as_str()), cmd.pos)
            } else {
                panic!("Not a dot expression")
            }
        } else {
            panic!("Not an import command")
        }
    }
}
