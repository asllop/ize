use crate::{
    ast::{Ast, CommandSet, ImportPath, SymType},
    lexer::Lexer,
    parser::Parser,
    semanter, BuildErr, IzeErr, Pos,
};
use alloc::string::String;

type ImportReaderFn = fn(&ImportPath, Pos) -> Result<String, IzeErr>;

/// Build IZE program.
pub fn build<'a>(code: &'a str, source: ImportPath, reader: ImportReaderFn) -> Result<Ast, IzeErr> {
    let mut ast = Ast {
        commands: Default::default(),
        imports: Default::default(),
        symbols: Default::default(),
        source,
    };

    let mut parser = Parser::new(Lexer::new(code).tokenize()?);

    while !parser.ended() {
        let command = parser.command()?;

        if let CommandSet::Import(import) = command.command {
            //TODO: detect circular import references: every time we import, annotate the file absolute path, and see if we repeat one, that means we have a cycle.
            // Import packages
            for pkg in import.packages {
                let mut pkg_ast =
                    build(&reader(&pkg.path, command.pos)?, pkg.path.clone(), reader)?;
                pkg_ast.source = pkg.path.clone();
                let mod_name = pkg
                    .mod_name()
                    .or_else(|err_msg| Result::ize_err(err_msg.into(), command.pos))?;
                if ast.imports.contains_key(&mod_name) {
                    return Result::ize_err(
                        format!("Module name {} already defined", mod_name),
                        command.pos,
                    );
                }
                ast.imports.insert(mod_name, pkg_ast);
            }
        } else {
            // Update symbol table
            match &command.command {
                CommandSet::Model(model) => {
                    check_symbol_exists(&ast, &model.name, command.pos)?;
                    ast.symbols.insert(model.name.clone(), SymType::Model);
                }
                CommandSet::Transfer(transfer) => {
                    check_symbol_exists(&ast, &transfer.name, command.pos)?;
                    ast.symbols.insert(transfer.name.clone(), SymType::Transfer);
                }
                CommandSet::Pipe(pipe) => {
                    check_symbol_exists(&ast, &pipe.name, command.pos)?;
                    ast.symbols.insert(pipe.name.clone(), SymType::Pipe);
                }
                CommandSet::Import(_) => {
                    return Result::ize_err("FATAL: This can't be an import!".into(), command.pos)
                }
            }
            ast.commands.push(command);
        }
    }

    // Pass the semantic cheker
    //semanter::sem_check(&ast)?;

    //TODO: call the transpiler: we will need to extend the "Import Reader", and make it a type that implements a trait to do multipe things: get imports, write transpiler files, etc.
    //TODO: just return Result<(), IzeErr>

    Ok(ast)
}

fn check_symbol_exists(ast: &Ast, sym: &str, pos: Pos) -> Result<(), IzeErr> {
    if ast.symbols.contains_key(sym) {
        Result::ize_err(format!("Symbol {} already exists", sym), pos)
    } else {
        Ok(())
    }
}
