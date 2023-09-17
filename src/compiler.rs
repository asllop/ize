use crate::{
    ast::{Ast, CommandSet, ImportPath},
    lexer::Lexer,
    parser::Parser,
    BuildErr, IzeErr, Pos,
};
use alloc::string::String;

type ImportReaderFn = fn(&ImportPath, Pos) -> Result<String, IzeErr>;

/// Build IZE program.
pub fn build<'a>(code: &'a str, reader: ImportReaderFn) -> Result<Ast, IzeErr> {
    let mut ast = Ast {
        commands: Default::default(),
        imports: Default::default(),
    };

    let mut parser = Parser::new(Lexer::new(code).tokenize()?);

    while !parser.ended() {
        let command = parser.command()?;
        if let CommandSet::Import(import) = command.command {
            for pkg in import.packages {
                let pkg_ast = build(&reader(&pkg.path, command.pos)?, reader)?;
                let alias = pkg.alias.unwrap_or_else(|| {
                    //TODO: We have to get the alias from the package last component?
                    //      Or we put all in global scope? Go approach vs Rust approach.
                    todo!("Alias is not defined")
                });
                if ast.imports.contains_key(&alias) {
                    return Result::ize_err(
                        format!("Module name {} already defined", alias),
                        command.pos,
                    );
                }
                ast.imports.insert(alias, pkg_ast);
            }
        } else {
            ast.commands.push(command);
        }
    }

    //TODO: call the semantic checker
    //TODO: call the transpiler: we will need to extend the "Import Reader", and make it a type that implements a trait to do multipe things: get imports, write transpiler files, etc.
    //TODO: just return Result<(), IzeErr>

    Ok(ast)
}
