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
                let alias = if let Some(alias) = pkg.alias {
                    alias
                } else {
                    if let ImportPath::Dot(dot_path) = pkg.path {
                        if let Some(last_compo) = dot_path.path.last() {
                            last_compo.clone()
                        } else {
                            // This should never happen, the parser enforces a non empty dot path.
                            return Result::ize_err("Dot paths can't be empty".into(), command.pos);
                        }
                    } else {
                        // This should never happen, the parser enforces all absolute paths to have an alias.
                        return Result::ize_err(
                            "Absolute paths must define an alias enforced by the parser".into(),
                            command.pos,
                        );
                    }
                };
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
