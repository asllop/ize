//! Semantic Analyzer
//!
//! Performs semantic analysis: symbol resolution (models, transfers, variables, and pipes), and type checking of all operations.

//TODO: Check reserved identifiers: no let, model, transfer or pipe is named "in" or "New".

//TODO: Check compount type integrity:
//  - Mux contains no repeated types.
//  - List contains only one type.
//  - Map contains one or two types.

//TODO: Check model integrity:
//  - Only one "..." field that comes the last.
//  - No field is of type None.
//  - Custom types actually exist.

//TODO: Check transfer integrity:
//  - Input and output custom types actually exist.
//  - No input and output type is None.

//TODO: Calculate the resulting type of every expression and annotate it in the AST.

//TODO: Check if the types match as they should:
//  - Math/logic operations are performed using the correct types.
//  - If-else condition contains an expression of type Boolean.
//  - If and else bodies are expressions of the same type.
//  - Select/Unwrap arms are expressions of the same type.
//  - Select's condition and arm's left side are of the same type.
//  - Resulting type of expressions matches the parent expression (transfer output, model structure, select/unwrap/if-else result, etc).

//TODO: Check variables:
//  - They exist when used and the type is correct.

//TODO: Unwrap:
//  - Left side of arm is a type.
//  - All possibilities are covered with corresponding arms.

//TODO: Select
//  - Left side of arm is a literal.
//  - All possibilities are covered with corresponding arms.

//TODO: Create a way to check method type checking.
//  - For exaple "any_string.Match()" must contain a single argument of type String.
//  - Not only for internal methods, also for extensions made in Rust (see "ext" module).

//TODO: Check dot expressions and function calls.
//  - All components of the dot actually exist.
//  - All function calls actually exist and the argument number of type are correct.
