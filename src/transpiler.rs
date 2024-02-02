//! # Transpiler
//!
//! Takes an IZE AST and converts it into Rust code.
//! 
//! The transpiler works in two steps:
//! 
//! - From the IZE AST, generate an intermediate AST that is closer to structured, imperative languages, like Rust (or Java, Go, JS, Python, etc).
//! - From the intermediate AST, generate the target language's AST (Rust), and finally deserialize it into code.

use crate::{ast::AstNode, err::IzeErr};
use alloc::{string::String, vec::Vec};

//TODO: El transpilador ha de prendre també la SF per saber exactament què és cada símbol que apareix dins una expressió.
//      Per exemple, si tenim un model anomenat A, també tenim una transfer del mateix nom. Si passem al símbol "A" com a argument
//      d'una trnsfer, a quà fem referència, a la transfer o al model? Quin symbol resolem dependrà del tipus del paràmetre, si
//      és "Type" o és "Traf".

/// Transpile an AST.
pub fn transpile(_ast: Vec<AstNode>) -> Result<String, IzeErr> {
    todo!()
}

/*
Transpilation Process:

- Types Str, Int, Float, Bool, List, Map, Tuple and Traf translate into rust equivalents: String, i64, f64, bool, Vec, HashMap, tuple and fn.
- Mux translates into an enum.
- None and Null types and values translate into None and Null empty structs in rust.
- Each import cmd translates into a use statement.
- Each model cmd translates into a struct and a constructor function.
- Each transfer cmd translates into a function.
    - Chain expr translate into block expressions.
    - Let expr translate into let statements.
    - Pair expr translate into struct initialization.
    - Arithmetic and logic expr translate into rust counterparts.
    - Call and dot expr translate into rust counterparts.
    - Group and primary expr translate into rust counterparts.
    - If-else expr translate into rust counterparts.
    - Select/Unwrap expr translate into match.
    - Type expr don't translate yet. We will see how we can use them.
- Each const cmd translates into a const.
- Each pipe cmd translates into a function.
- Run cmd translates into the main function.

NOTES:  All identifiers are prefixed. For example, a model named MyModel will translate into a rust struct named Model_MyModel
        and a constructor function called Transfer_MyModel.
        Put in the top of file:
            #![allow(non_camel_case_types)]
            #![allow(non_snake_case)]
        To avoid naming warnings. And maybe also:
            #![allow(dead_code)]

        There is a module with definitions that all generated rs files will use. It contains:
            - Primitive types like None, Null, List, Map, and constructors.
            - Primitive functions/methods.
            - Some useful constants and reimports.
 */

 //TODO: create an AST type for Rust that accepts all parts of rust grammar and converts itself into rust code.