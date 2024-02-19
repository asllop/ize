use std::{
    env,
    fs::File,
    io::{prelude::*, BufReader},
    str,
};

use ize::{
    ast::{Ast, Command, CommandKind, ExpressionKind, ImportRef, ImportSymbol, Primary},
    grammar_cmd, lexer,
    pos::RangePos,
    semcheck,
};
use rustc_hash::FxHashMap;

//TODO: detect import cycles.

fn main() {
    let file_path = "izeware/experiment_semcheck.iz";
    let ast = parse_and_import(absolute_path(file_path).as_str());

    println!("\n============ AST ============\n\n");
    println!("{:#?}", ast);

    recursive_check_ast(&ast);
}

fn recursive_check_ast(ast: &Ast) {
    let symtab = semcheck::check_ast(&ast).expect("Error semchecking the AST");

    println!("\n========== SEMCHECKER ==========\n\n");
    println!("Symbol table = {:#?}", symtab);

    for import_ast in &ast.imports {
        recursive_check_ast(import_ast);
    }
}

//TODO: move all this code into the crate, we could call it "loader" module. Only leave the file read here and absolute path.
//      Use a Path type instead of a String, is more portable.

fn absolute_path(path_str: &str) -> String {
    let mut path_buf = env::current_dir().expect("Error getting current dir");
    path_buf.push(path_str);
    path_buf.to_string_lossy().into()
}

// Parse file, then generate imports and parse imported files too.
fn parse_and_import(file_path: &str) -> Ast {
    let commands = parse_file(file_path);

    // Segregate imports from other commands
    let (imports, commands): (Vec<_>, Vec<_>) = commands.into_iter().partition(|cmd| {
        if let CommandKind::Import { .. } = &cmd.kind {
            true
        } else {
            false
        }
    });

    semcheck::check_import_commands(&imports).expect("Semantic checker for import commands failed");

    // Convert import commands into import paths.
    let import_path_vec: Vec<ImportPaths> = imports.into_iter().map(|cmd| cmd.into()).collect();

    println!("\n========== COMMANDS ({file_path}) ==========\n\n");

    for cmd in &commands {
        println!("----------------> {:#?}\n", cmd);
    }

    println!("\n========== IMPORTS ({file_path}) ==========\n\n");

    println!("{:#?}", import_path_vec);

    let mut imports = vec![];
    let mut imported_symbols = FxHashMap::default();
    // Parse imported files
    for (index, import_path) in import_path_vec.into_iter().enumerate() {
        let file_path = import_path.path + ".iz";
        let symbols = import_path.symbols;
        let ast = parse_and_import(file_path.as_str());
        //TODO: if imported symbol is "*", we have to import all symbols from the AST.
        for sym in symbols {
            let symbol = sym.symbol.id;
            let rename = if let Some(rename) = sym.rename {
                Some(rename.id)
            } else {
                None
            };
            imported_symbols.insert(symbol, ImportRef::new(index, rename, import_path.pos));
        }
        imports.push(ast);
    }

    Ast::new(file_path.into(), commands, imports, imported_symbols)
}

fn parse_file(file_path: &str) -> Vec<Command> {
    let file = File::open(file_path).expect("Error opening file");
    let mut reader = BufReader::new(file);
    let mut buf = Vec::<u8>::new();
    reader.read_to_end(&mut buf).expect("Error reading");

    let code = str::from_utf8(&buf).expect("Error converting buffer to UTF-8");
    let tokens = lexer::tokenize(code).expect("Bad token");
    let mut input = tokens.as_slice();

    let mut commands = vec![];

    while !input.is_empty() {
        let (rest, parsed, _) = grammar_cmd::cmd(input).expect("Error parsing command");
        let parsed = parsed.cmd().expect("Node must be a command");
        input = rest;
        commands.push(parsed);
    }

    commands
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
            match path.kind {
                ExpressionKind::Primary(Primary::Identifier(id)) => {
                    Self::new(symbols, absolute_path(id.as_str()), cmd.pos)
                }
                ExpressionKind::Dot(dot_path_vec) => {
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
                }
                _ => panic!("Not a dot expression"),
            }
        } else {
            panic!("Not an import command")
        }
    }
}
