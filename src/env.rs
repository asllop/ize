use crate::lexer::TokenData;
use alloc::string::String;
use rustc_hash::FxHashMap;

pub struct EnvErr {
    pub message: String,
}

enum ValType {
    Variable,
    Constant,
    //TODO: other types: structs, transfers, etc
}

#[derive(Default)]
pub struct Environment {
    //TODO: distinguish variables, contants and other things. Scope.
    values: FxHashMap<String, (TokenData, ValType)>,
}

impl Environment {
    pub fn assign_var(&mut self, name: &String, val: TokenData) -> Result<(), EnvErr> {
        match self.values.get(name) {
            Some((_, ValType::Variable)) | None => {
                self.values.insert(name.clone(), (val, ValType::Variable));
                Ok(())
            }
            _ => Err(EnvErr {
                message: format!("Identifier '{}' is not a variable", name),
            }),
        }
    }

    pub fn get_val(&mut self, name: &String) -> Option<&TokenData> {
        match self.values.get(name) {
            Some((v, ValType::Variable | ValType::Constant)) => Some(v),
            _ => None,
        }
    }

    pub fn def_const(&mut self, name: &String, val: TokenData) {
        self.values.insert(name.clone(), (val, ValType::Constant));
    }
}
