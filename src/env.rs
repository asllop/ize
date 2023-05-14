use crate::lexer::TokenData;
use alloc::string::String;
use rustc_hash::FxHashMap;

pub struct EnvErr {
    pub message: String,
}

pub enum ElemType {
    Variable,
    Constant,
    //TODO: other types: structs, transfers, etc
}

#[derive(Default)]
pub struct Environment {
    //TODO: distinguish variables, contants and other things. Scope.
    variables: FxHashMap<String, (TokenData, ElemType)>,
}

impl Environment {
    pub fn assign_var(&mut self, name: &String, val: TokenData) -> Result<(), EnvErr> {
        match self.variables.get(name) {
            Some((_, ElemType::Variable)) | None => {
                self.variables.insert(name.clone(), (val, ElemType::Variable));
                Ok(())
            }
            _ => Err(EnvErr { message: format!("Identifier '{}' is not a variable", name) }),
        }
    }

    pub fn get_val(&mut self, name: &String) -> Option<&TokenData> {
        match self.variables.get(name) {
            Some((v, ElemType::Variable | ElemType::Constant)) => Some(v),
            _ => None,
        }
    }

    pub fn def_const(&mut self, name: &String, val: TokenData) {
        self.variables.insert(name.clone(), (val, ElemType::Constant));
    }
}