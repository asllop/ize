/*
Funcionalitats que volem:

- Poder encadenar tokens. Dir-li al parser que una determinada seqüència de tokens és una expressió/comanda.
  Exemple: "let" ID expr
- Poder saber que una seqüència pot ser-hi o no. Això implica que la seqüència ha de ser capaç de trobar-se dins l'stream de tokens.
  Ens cal alguna mena de funció de comprovació/test que ens ho digui.
- Generació automàtica d'errors. Quan una seqüència falla, ha de saber en quin entorn es trobava, per exemple dins un SELECT
  i generar un error que ens digui el motiu (missatge), la posició (lloc dins el codi) i context (SELECT, en aquest cas).
  El context és una pila, pot ser un error generat dins una expressió LET, dins un SELECT, dins una TRANSFER.
- Poder generar seqüències complexes amb elements opcionals i iteratius: [0,1] ; [1, N] ; [0, N]
  Exemples: "run"? , (ID ":" expr)*  , (expr "->" expr)+
- Recollir dades al final de tota la seqüència i generar una estructura/element de l'AST. Per exemple, amb una expressió LET, al final
  hem de poder recollir el valor de ID, de expr i la pos d'inici per poder generar la estructura Expr. Aquesta collita de dades
  pot incloure elements opcionals i iteratius, per tant cal alguna interfícia complexa d'extracció.

Exemple "select":

SELECT ->		"select" expr "as" ID
                "("
                    expr "->" expr
                    ("," expr "->" expr)*
                ")"

                let expr = Parser::new(&mut token_stream)
                    .tk(SELECT).expression().drop(AS).identifier()
                        .drop(OPEN_PARENTH)
                            .expression().drop(ARROW).expression()
                            .mul(|parser| {
                                parser
                                    .drop(COMMA).expression().drop(ARROW).expression()
                            })
                        .drop(CLOSE_PARENTH)
                    .collect(|parser| {
                        // collect results and generate AST object or error
                    })?;

                "parser" dins de collect() conté tota la pila de resultats de cada pas descrit:

                1. Pos i token de SELECT.
                2. Pos i expressió
                3. Pos i identificador.
                4. Pos i expressió
                5. Pos i expressió
                6. Pos i Vector (pot estar buit):
                    1. Pos i expressió
                    2. Pos i expressió
                    ...

                Ens cas de produir-se un error en algun dels passos, es genera un IzeErr i es passa cap avall fins arribar al collect.

Mètodes del parser:

tk()            Parsa un token.
drop()          Igual que tk(), però no inclou el token al resultat final.
tks()           Una llista de tokens pot apareixer en aquesta posició.
expr()          Parsa una expressió.
identifier()    Parsa un identificador.
literal()       Parsa un literal.
mul()           Parsa zero o més vegades una seqüència.
plu()           Parsa una o més vegades una seqüència.
opt()           Parsa zero o una vegada una seqüència.
or()            Una llista de seqüències pot apareixer en aquesta posició.

COM IMPLEMENTEM LA PRECEDENCIA?

Quan generem ExprParser li podem passar un arg extra a new() per dir-li quina expressió som (TERM, UNARY, SELECT, etc).
Aleshores, en comptes de cridar expression() podem cridar precedent_expr() i provarà de parsar la següent expressió
que segueix a l'actual en precedència. Prèviament hem d'haver registrat totes les funcions que parsen expressions
en un selector de precedència.

--> Una altra possibilitat és que expression() prengui com argument una funció que parseja una expressió i aquesta és la seva precedència.
--> També podem tenir un "else()" que ens digui per on hem de seguir en precedència si no trobem una expressió del tipus actual.

GRAMÀTICA
---------

Definitions:

ID ->			    Alphanumeric or "_". Can’t start with numeric.
literal ->		    INTEGER | FLOAT | STRING | "true" | "false" |
                    "null" | "none"
type ->		        ID ("[" type ("," type)* "]")?

Expressions:

expr ->             chain
chain ->            let ";" let (";" let)*
let ->              "let" ID let
select ->           "select" expr "as" ID "("
                        expr "->" expr
                        ("," expr "->" expr)*
                    ")"
unwrap ->           "unwrap" expr "as" ID "("
                        expr "->" expr
                        ("," expr "->" expr)*
                    ")"
ifelse ->		    "if" "(" expr ")" expr "else" expr
equality ->         comparison ( ( "!=" | "==" ) comparison )*
comparison ->       term ( ( ">" | ">=" | "<" | "<=" ) term )*
term ->             factor ( ( "-" | "+" ) factor )*
factor ->           unary ( ( "/" | "*" ) unary )*
unary ->            ( "!" | "-" ) unary | dot
dot ->              (ID | call) ("." (ID | call))+
call ->             ID "(" (expr ("," expr)*)? ")"
group ->            "(" expr ")"
primary ->          literal | ID | type

Expressions (alternatiu):

expr ->             chain
chain ->            expr ";" expr (";" expr)*
let ->              "let" ID select
select ->           "select" expr "as" ID "("
                        expr "->" expr
                        ("," expr "->" expr)*
                    ")"
unwrap ->           "unwrap" expr "as" ID "("
                        expr "->" expr
                        ("," expr "->" expr)*
                    ")"
ifelse ->		    "if" "(" expr ")" expr "else" expr
equality ->         expr ( "!=" | "==" ) expr
comparison ->       expr ( ">" | ">=" | "<" | "<=" ) expr
term ->             expr ( "-" | "+" ) expr
factor ->           expr ( "/" | "*" ) expr
unary ->            ( "!" | "-" ) expr
dot ->              (ID | call) ("." (ID | call))+
call ->             ID "(" (expr ("," expr)*)? ")"
group ->            "(" expr ")"
primary ->          literal | ID | type

//NOTE: AstNode is either an Expr or a Command.

fn chain_expr(mut tokens: &TokenStream) -> Result<AstNode, IzeErr> {
    Parser::new(tokens)
        .expr(equality_expr)
        .mul(|parser| {
            parser
                .tk(SEMICOLON)
                .expr(equality_expr)
        })
        .collect(|parser| {
            // extract data from "parser" and generate AstNode
        })
}

fn equality_expr(mut tokens: &TokenStream) -> Result<AstNode, IzeErr> {
    Parser::new(tokens)
        .expr(comparison_expr)
        .mul(|parser| {
            parser
                .tks(&[NOT_EQUAL, TWO_EQUALS])
                .expr(comparison_expr)
        })
        .collect(|parser| {
            // extract data from "parser" and generate AstNode
        })
}

fn comparison_expr(mut tokens: &TokenStream) -> Result<AstNode, IzeErr> {
    Parser::new(tokens)
        .expr(term_expr)
        .mul(|parser| {
            parser
                .tks(&[NOT_EQUAL, TWO_EQUALS])
                .expr(term_expr)
        })
        .collect(|parser| {
            // extract data from "parser" and generate AstNode
        })
}

// ...

//TODO
fn primary_expr(mut tokens: &TokenStream) -> Result<AstNode, IzeErr> {
    Parser::new(tokens)
        .or(&[
            |parser| parser.literal(),
            |parser| parser.identifier(),
            |parser| parser.expr(type_expr),
        ])
        .collect(|parser| {
            // ...
        })
}

//TODO: les funcions "expr" consumeixen un Grammar i retornen un Grammar o error.


PROBLEMA

Ens cal una manera de comprovar si una estructura de tokens existeix sense modificar el vector de tokens i sense generar un error.
Per exemple, SELECT, hem de mirar si "select" existeix i si no, simplement seguim amb la precedència d'expressions.
Amb els operadors binaris i chain igual, si existeix l'operador bé, si no, seguim amb la precedència,
però no generem un error si no trobem l'operador.
En canvi en altres cass si que volem generar error, per exemple si no trobem l'AS del select.

Ens cal diferenciar entre "checkers" i "asserters". O simplement deixem que decideixi "collect". Quan un tk,tks, etc, falla, cridem
directament a "collect" i aquest decideix que fer. Si veu que ha fallat el primer "select", simplement crida la següent expressió.

fn let_expr(tokens) -> Result<Expr, IzeErr> {
    Grammar::new(tokens)
        .tk(LET).identifier().expr(whatever_expr)
        .collect(|result| {
            //TODO: check if ok or fail and where it failed. If LET failed, it's not a let expr, try next expression.
            //      either return Result<Expr, IzeErr> or call next expression in precedence
        })
}

fn let_expr(tokens) -> Result<Expr, IzeErr> {
    Grammar::new(tokens,
        &[
            Elem::Tk(Let),
            Elem::Identifier,
            Elem::Expr(whatever_expr)
        ]
    ).collect(|result| {
        //TODO: check if ok or fail and where it failed. If LET failed, it's not a let expr, try next expression.
        //      either return Result<Expr, IzeErr> or call next expression in precedence
    })
}

També podem tenir dos modes a la mateixa cadena, mode check i mode collect. El primer només mira si els tokens i són en l'ordre
que els hem indicat. El segon mode, extrau els tokens. La idea és definir una cadena un cop i que cada element sàpiga si es troba en mode
check o collect i faci la feina.

TODO: how to pass tokens through the process?? it must go though the Expr functions and return back.

*/

use std::ops::RemAssign;

use crate::{
    ast::{Expr, ExprSet, Literal},
    lexer::{
        TokenKind::{self, *},
        TokenStream,
    },
    parser::common::FromToken,
    BuildErr, IzeErr, Pos,
};
use alloc::{boxed::Box, string::String, vec::Vec};
use Elem::*;

type GrammarFn = fn() -> Grammar;

/// Grammar elements.
#[derive(Debug)]
pub enum Elem {
    /// One token.
    Tk(TokenKind),
    /// Multiple possible tokens.
    OrTk(&'static [TokenKind]),
    /// Identifier.
    Identifier,
    /// Literal.
    Literal,
    /// A grammar sequence present zero or more times.
    Mul(&'static [Elem]),
    /// A grammar sequence present one or more times.
    Plu(&'static [Elem]),
    /// A grammar sequence present zero or one time.
    Opt(&'static [Elem]),
    /// Select one out of multiple possible grammar sequences. The first that matches.
    Sel(&'static [&'static [Elem]]),
    /// Expression defined by function.
    Expr(GrammarFn),
    /// This element makes the parsing of current Grammar unavoidable. Once found we can't fallaback to the next expression.
    Must(&'static Elem),
}

/*

/// Define language grammars.
pub struct Grammar<'a> {
    token_stream: &'a mut TokenStream,
}

impl<'a> Grammar<'a> {
    /// Create a new grammar object.
    pub fn new(token_stream: &'a mut TokenStream) -> Self {
        Self { token_stream }
    }

    /// Parse using grammar rule set.
    pub fn parse(&mut self, grammar: &'static [Elem]) -> GrammarCollector {
        let mut result = ParseResult {
            succeed: true,
            error: None,
            atoms: vec![],
        };
        self.exec_grammar(&mut result, grammar);
        GrammarCollector::new(result, self.token_stream)
    }

    fn exec_grammar(&mut self, result: &mut ParseResult, grammar: &'static [Elem]) -> bool {
        for elem in grammar {
            match elem {
                Elem::Tk(token_kind) => {
                    if self.token_stream.is_token(*token_kind, 0) {
                        let (_, pos) = self
                            .token_stream
                            .consume_token()
                            .into_particle()
                            .expect("The stream should contain a particle");
                        result.atoms.push(ResultAtom::Token {
                            pos,
                            val: *token_kind,
                        });
                    } else {
                        self.result_fail(result, "The expected token wasn't found");
                        return false;
                    }
                }
                Elem::OrTk(tokens) => {
                    if self.token_stream.check_tokens(*tokens, 0) {
                        let (token_kind, pos) = self
                            .token_stream
                            .consume_token()
                            .into_particle()
                            .expect("The stream should contain a particle");
                        result.atoms.push(ResultAtom::Token {
                            pos,
                            val: token_kind,
                        });
                    } else {
                        self.result_fail(result, "None of the expected tokens was found");
                        return false;
                    }
                }
                Elem::Identifier => {
                    if self.token_stream.is_ident(0) {
                        let (id, pos) = self
                            .token_stream
                            .consume_token()
                            .into_ident()
                            .expect("The stream should contain an identifier");
                        result.atoms.push(ResultAtom::Ident { pos, val: id });
                    } else {
                        self.result_fail(result, "Expected an identifier and wasn't found");
                        return false;
                    }
                }
                Elem::Literal => {
                    if self.token_stream.is_literal(0) {
                        let (lit, pos) = self
                            .token_stream
                            .consume_token()
                            .into_literal()
                            .expect("The stream should contain a literal");
                        match lit {
                            Literal::Integer(i) => {
                                result.atoms.push(ResultAtom::IntLiteral { pos, val: i });
                            }
                            Literal::Float(f) => {
                                result.atoms.push(ResultAtom::FltLiteral { pos, val: f });
                            }
                            Literal::String(s) => {
                                result.atoms.push(ResultAtom::StrLiteral { pos, val: s });
                            }
                            Literal::Boolean(b) => {
                                result.atoms.push(ResultAtom::BoolLiteral { pos, val: b });
                            }
                            Literal::None => {
                                result.atoms.push(ResultAtom::NoneLiteral { pos });
                            }
                            Literal::Null => {
                                result.atoms.push(ResultAtom::NullLiteral { pos });
                            }
                        }
                    } else {
                        self.result_fail(result, "Expected a literal and wasn't found");
                        return false;
                    }
                }
                Elem::Mul(_) => todo!("Mul grammar"),
                Elem::Plu(_) => todo!("Plu grammar"),
                Elem::Opt(_) => todo!("Opt grammar"),
                Elem::OrEl(_grammars) => {
                    // for &grammar in *grammars {
                    //     self.exec_grammar(result, grammar);
                    // }
                    todo!("OrEl grammar")
                }
                Elem::Expr(expr_fn) => match expr_fn(self.token_stream) {
                    Ok(expr) => {
                        result.atoms.push(ResultAtom::Expr {
                            pos: expr.pos,
                            val: expr,
                        });
                    }
                    Err(ize_err) => {
                        result.succeed = false;
                        result.error = Some(ize_err);
                        return false;
                    }
                },
            }
        }
        true
    }

    fn result_fail(&self, result: &mut ParseResult, msg: &str) {
        result.succeed = false;
        result.error = Some(IzeErr {
            message: msg.into(),
            pos: self.token_stream.last_pos(),
        });
    }
}

/// Grammar collector type. Returned by [parse()](Grammar).
pub struct GrammarCollector<'a> {
    result: ParseResult,
    token_stream: &'a mut TokenStream,
}

impl<'a> GrammarCollector<'a> {
    /// Create a new grammar collector.
    pub fn new(result: ParseResult, token_stream: &'a mut TokenStream) -> Self {
        Self {
            result,
            token_stream,
        }
    }

    /// Collect parsing results and generate AST component.
    pub fn collect(
        self,
        block: fn(ParseResult, &mut TokenStream) -> Result<Expr, IzeErr>,
    ) -> Result<Expr, IzeErr> {
        block(self.result, self.token_stream)
    }
}

///////// TEST expression parsing \\\\\\\\\

pub fn expression(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Expression");
    let_expr(token_stream)
}

fn let_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Let Expr");
    Grammar::new(token_stream)
        .parse(&[
            Tk(Let),
            Identifier,
            Expr(let_expr)
        ])
        .collect(|mut result, token_stream| {
            if result.succeed {
                // Build LET expression
                let let_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let let_ident = result.atoms.pop().unwrap().as_ident().unwrap();
                let pos = result.atoms.pop().unwrap().pos();

                let expr = Expr::new(
                    ExprSet::Let {
                        name: let_ident,
                        value: Box::new(let_expr),
                    },
                    pos,
                );

                println!("---> IS a Let Expr = {:?}", expr);

                Ok(expr)
            } else {
                if result.atoms.is_empty() {
                    // Not a LET expr, keep parsing in precedence order
                    ifelse_expr(token_stream)
                } else {
                    // Error parsing LET expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        })
}

fn ifelse_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# IfElse Expr");
    Grammar::new(token_stream)
        .parse(&[
            Expr(term_expr),
            Tk(If),
            Expr(expression),
            Tk(Else),
            Expr(expression),
        ])
        .collect(|mut result, _| {
            if result.succeed {
                // Build if-else expression
                let else_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let _else_token = result.atoms.pop().unwrap().as_token().unwrap();
                let then_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let _if_token = result.atoms.pop().unwrap().as_token().unwrap();
                let cond_expr_atom = result.atoms.pop().unwrap();
                let pos = cond_expr_atom.pos();
                let cond_expr = cond_expr_atom.as_expr().unwrap();

                let expr = Expr::new(
                    ExprSet::IfElse {
                        condition: Box::new(cond_expr),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr)
                    },
                    pos,
                );

                println!("---> IS a IfElse Expr = {:?}", expr);

                Ok(expr)
            } else {
                if result.atoms.len() == 1 {
                    // Not an ifelse expression, return the remaining expression parsed
                    let expr = result.atoms.pop().unwrap().as_expr().unwrap();
                    Ok(expr)
                } else {
                    // Error parsing term expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        })
}

fn term_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Term Expr");
    Grammar::new(token_stream)
        .parse(&[
            Expr(factor_expr),
            OrTk(&[Plus, Minus]),
            Expr(expression)
        ])
        .collect(|mut result, _| {
            if result.succeed {
                // Build term-binary expression
                let right_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let operator = result.atoms.pop().unwrap().as_token().unwrap();
                let left_expr_atom = result.atoms.pop().unwrap();
                let pos = left_expr_atom.pos();
                let left_expr = left_expr_atom.as_expr().unwrap();

                let expr = Expr::new(
                    ExprSet::Binary {
                        op: operator.try_into().unwrap(),
                        left_expr: Box::new(left_expr),
                        right_expr: Box::new(right_expr),
                    },
                    pos,
                );

                println!("---> IS a Term Expr = {:?}", expr);

                Ok(expr)
            } else {
                if result.atoms.len() == 1 {
                    // Not a term expression, return the remaining expression parsed
                    let expr = result.atoms.pop().unwrap().as_expr().unwrap();
                    Ok(expr)
                } else {
                    // Error parsing term expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        })
}

fn factor_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Factor Expr");
    Grammar::new(token_stream)
        .parse(&[
            Expr(group_expr),
            OrTk(&[Star, Slash, Percent]),
            Expr(expression),
        ])
        .collect(|mut result, _| {
            if result.succeed {
                // Build factor-binary expression
                let right_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let operator = result.atoms.pop().unwrap().as_token().unwrap();
                let left_expr_atom = result.atoms.pop().unwrap();
                let pos = left_expr_atom.pos();
                let left_expr = left_expr_atom.as_expr().unwrap();

                let expr = Expr::new(
                    ExprSet::Binary {
                        op: operator.try_into().unwrap(),
                        left_expr: Box::new(left_expr),
                        right_expr: Box::new(right_expr),
                    },
                    pos,
                );

                println!("---> IS a factor = {:?}", expr);

                Ok(expr)
            } else {
                if result.atoms.len() == 1 {
                    // Not a factor expression, return the remaining expression parsed
                    let expr = result.atoms.pop().unwrap().as_expr().unwrap();
                    Ok(expr)
                } else {
                    // Error parsing term expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        })
}

fn group_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Group Expr");
    Grammar::new(token_stream)
        .parse(&[
            Tk(OpenParenth),
            Expr(expression),
            Tk(ClosingParenth)
        ])
        .collect(|mut result, token_stream| {
            if result.succeed {
                // Build group expression
                let _right_parenth = result.atoms.pop().unwrap();
                let inner_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let pos = result.atoms.pop().unwrap().pos();

                let expr = Expr::new(
                    ExprSet::Group {
                        expr: Box::new(inner_expr),
                    },
                    pos,
                );

                println!("---> IS a Group Expr = {:?}", expr);

                Ok(expr)
            } else {
                if result.atoms.is_empty() {
                    // Not a group expression, try to parse a primary expression
                    primary_expr(token_stream)
                } else {
                    // Error parsing term expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        })
}

// fn unary_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
//     Grammar::new(token_stream).parse(&[
//         OrTk(&[Minus, Not]),
//         Expr(primary_expr),
//         // OrEl(&[
//         //     &[Expr(unary_expr)],
//         //     &[Expr(primary_expr)],
//         // ]),
//     ]).collect(|result, token_stream| {
//         todo!("Collect factor expr")
//     })
// }

fn primary_expr(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    println!("# Primary Expr");
    if token_stream.is_literal(0) {
        // Literal
        let (lit, pos) = token_stream.consume_token().into_literal()?;
        let expr = Expr::new(ExprSet::Literal(lit), pos);

        println!("---> IS a Primary Expr = {:?}", expr);

        Ok(expr)
    } else if token_stream.is_token(TokenKind::Ident, 0) {
        // Identifier
        let (id, pos) = token_stream.consume_token().into_ident()?;
        let expr = Expr::new(ExprSet::Identifier(id), pos);

        println!("---> IS a Primary Expr = {:?}", expr);

        Ok(expr)
    } else {
        Result::ize_err(
            "Couldn't parse a valid expression at end".into(),
            token_stream.last_pos(),
        )
    }
    // Grammar::new(token_stream).parse(&[
    //     OrEl(&[
    //         &[Literal],
    //         &[Identifier],
    //         //TODO: type expression
    //     ])
    // ]).collect(|result| {
    //     todo!("Collect primary expr")
    // })
}
*/

/*
PROBLEMA amb Mul, Plu, Opt i OrEl:
Hem de comprovar si alguna de les seqüencies funciona, però sense modificar el TokenStream fins que no sapiguem segur que funcionarà.

Podem definir un sistema de check, per saber si una expressió existeix abans de parsar i consumir tokens.
Les funcions "expr" en comptes de retornar un Result<Expr, IzeErr>, retorna una struct tipus Grammar.
Aquesta instància de Grammar conté dues coses definides per l'usuari:
    - Un array de definició de gramàtica.
    - Un closure per fer el collect.
El procés de parsing aleshores consisteix en:
    - Recursivament anar comprovant tots els elements definits a la gramàtica contra el TokenStream, sens modificar-lo, mitjançant un índex que indica la pos actual.
    - Un cop tot l'arbre d'expressions ha retornat OK, fem el parsing de veritat consumint tokens i cridem les funcions "collect".
*/

pub struct Grammar {
    grammar: &'static [Elem],
    next: Option<GrammarFn>,
    collector: fn(ParseResult, &mut TokenStream) -> Result<Expr, IzeErr>,
}

impl Grammar {
    /// Create new grammar to parse an expression.
    /// - grammar: Array of grammar elements.
    /// - next: Next expression to parse in precedence order in case 'grammar' doesn't match.
    /// - collector: Collector closure, to collect grammar elements and build expression.
    pub fn new(
        grammar: &'static [Elem],
        next: Option<GrammarFn>,
        collector: fn(ParseResult, &mut TokenStream) -> Result<Expr, IzeErr>,
    ) -> Self {
        Self {
            grammar,
            next,
            collector,
        }
    }

    /*  PROBLEMA: ERROR HANDLING

        Aquesta estructura basada en gramàtiques s'assembla força a un parser combinator.
        El problema és que no sabem que una expressió ha fallat fins que no ho hem provat tot, i per tant
        fer un bon error handling és complicat. Per exemple l'expressió errònia:
            a+b;let 10
        Produeix una expressió correcta "a+b" i l'índex de l'error és al ";".
        Amb el parser Recursive Descendant clàssic podem detectar l'error dins de let_expr() i generar un missatge tal com:
            "Identifier expected after let". Amb l'índex correcte apuntant al "10".
        El problema és que com que chain_expr() no pot parsar, perquè "let_expr" ha fallat, fallback a next i finalment es pot parsar
        una expressió binaria, de manera que l'error es produeix en intentar parsar la següent epxressió que comença per ";".
        Dita d'una altra manera, quan una gramàtica no pot parsar, no té manera de saber si és perquè l'expressió és errònia o
        simplement perquè correspon a un altre tipus d'expressió.

        SOLUCIÓ:

        Cal incloure informació sobre els checks a la gramàtica. Per exemple, a la gramàtica de let_expr li hem de poder dir
        que si troba un token "let" ja no pot fer fallaback a next, a partir d'aquest punt o bé parsa una expressió let completa
        o ha de generar un error. El mateix amb chain, si troba un ";", ha de poder parsar una expressió chain completa.
        Igual amb els binaris, si troba un "+" ha de poder trobar una altra expressió despres. Etc.
        Això ho podem fer amb un nou element gramatical: Must.
     */

    /// Check if a grammar can be parsed given the available tokens.
    pub fn check(&self, token_stream: &TokenStream, index: usize) -> (bool, usize) {
        let (result, new_index) = Self::check_grammar(token_stream, self.grammar, index);
        if result {
            (result, new_index)
        } else {
            // Try next in precedence
            match self.next {
                Some(next) => Self::check_grammar(token_stream, &[Expr(next)], index),
                None => (false, index),
            }
        }
    }

    /// Check a gramamar on a token stream starting at index.
    /// Returns (result, new_index):
    /// - result: Indicates whether the check was sucessful or not.
    /// - new_index: Is the current index on the token stream after the check, only if result == true.
    fn check_grammar(token_stream: &TokenStream, grammar: &[Elem], index: usize) -> (bool, usize) {
        let mut index = index;
        for elem in grammar {
            match elem {
                Elem::Tk(token_kind) => {
                    if token_stream.is_token(*token_kind, index) {
                        index += 1;
                    } else {
                        return (false, index);
                    }
                }
                Elem::OrTk(tokens) => {
                    if token_stream.is_any_of_tokens(*tokens, index) {
                        index += 1;
                    } else {
                        return (false, index);
                    }
                }
                Elem::Identifier => {
                    if token_stream.is_ident(index) {
                        index += 1;
                    } else {
                        return (false, index);
                    }
                }
                Elem::Literal => {
                    if token_stream.is_literal(index) {
                        index += 1;
                    } else {
                        return (false, index);
                    }
                }
                Elem::Mul(grammar) => loop {
                    let (check_result, new_index) =
                        Self::check_grammar(token_stream, grammar, index);
                    if !check_result {
                        break;
                    }
                    index = new_index;
                },
                Elem::Plu(grammar) => {
                    let mut i = 0;
                    loop {
                        let (check_result, new_index) =
                            Self::check_grammar(token_stream, grammar, index);
                        if !check_result {
                            break;
                        }
                        index = new_index;
                        i += 1;
                    }
                    if i == 0 {
                        return (false, index);
                    }
                }
                Elem::Opt(_) => todo!("Optional grammar element '?'"),
                Elem::Sel(grammars) => {
                    let mut grammar_result = false;
                    for &grammar in grammars.iter() {
                        let (check_result, new_index) =
                            Self::check_grammar(token_stream, grammar, index);
                        if check_result {
                            index = new_index;
                            grammar_result = true;
                            break;
                        }
                    }
                    // None of the grammars matched
                    if !grammar_result {
                        return (false, index);
                    }
                }
                Elem::Expr(expr) => {
                    let grammar = expr();
                    let (check_result, new_index) = grammar.check(token_stream, index);
                    if check_result {
                        index = new_index;
                    } else {
                        return (false, new_index);
                    }
                },
                Elem::Must(_) => todo!("Must grammar element")
            }
        }
        (true, index)
    }
}

/// Parsing results object.
pub struct ParseResult {
    /// Parsing succeeded.
    pub succeed: bool,
    /// If parsing failed, error object.
    pub error: Option<IzeErr>,
    /// Actual parsing results.
    pub atoms: Vec<ResultAtom>,
}

/// Parsing results component.
pub enum ResultAtom {
    Ident { pos: Pos, val: String },
    Token { pos: Pos, val: TokenKind },
    StrLiteral { pos: Pos, val: String },
    IntLiteral { pos: Pos, val: i64 },
    FltLiteral { pos: Pos, val: f64 },
    BoolLiteral { pos: Pos, val: bool },
    NoneLiteral { pos: Pos },
    NullLiteral { pos: Pos },
    Expr { pos: Pos, val: Expr },
}

impl ResultAtom {
    pub fn as_ident(self) -> Result<String, IzeErr> {
        if let ResultAtom::Ident { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an identifier ResultAtom".into(), self.pos())
        }
    }

    pub fn as_expr(self) -> Result<Expr, IzeErr> {
        if let ResultAtom::Expr { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an expression ResultAtom".into(), self.pos())
        }
    }

    pub fn as_token(self) -> Result<TokenKind, IzeErr> {
        if let ResultAtom::Token { val, .. } = self {
            Ok(val)
        } else {
            Result::ize_err("Expected an expression ResultAtom".into(), self.pos())
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            ResultAtom::Ident { pos, .. }
            | ResultAtom::Token { pos, .. }
            | ResultAtom::StrLiteral { pos, .. }
            | ResultAtom::IntLiteral { pos, .. }
            | ResultAtom::FltLiteral { pos, .. }
            | ResultAtom::BoolLiteral { pos, .. }
            | ResultAtom::NoneLiteral { pos }
            | ResultAtom::NullLiteral { pos }
            | ResultAtom::Expr { pos, .. } => *pos,
        }
    }
}

///////// TEST expression parsing \\\\\\\\\

pub fn parse_expression(token_stream: &mut TokenStream) -> Result<Expr, IzeErr> {
    todo!("Parsing not implemented yet")
}

pub fn check_expression(token_stream: &mut TokenStream, index: usize) -> (bool, usize) {
    let (result, new_index) = expr().check(token_stream, index);

    if result {
        print_token_range(token_stream, index, new_index);
    } else {
        println!("\n---> CHECK FAILED, next token: {:?}\n", token_stream.at(index).unwrap().lexeme);
    }
    println!("Check Result = {} , Index = {}", result, index);

    (result, new_index)
}

fn print_token_range(token_stream: &TokenStream, mut start: usize, end: usize) {
    print!("\n---> CHECKED EXPR: ");
    while start < end {
        let lexeme = &token_stream.at(start).unwrap().lexeme;
        print!("{:?} ", lexeme);
        start += 1;
    }
    println!("\n");
}

fn expr() -> Grammar {
    chain_expr()
}

// next_expr (";" next_expr)+
fn chain_expr() -> Grammar {
    Grammar::new(
        &[Expr(let_expr), Plu(&[Tk(Semicolon), Expr(let_expr)])],
        Some(let_expr),
        |mut result, token_stream| todo!(),
    )
}

// "let" ID let_expr
fn let_expr() -> Grammar {
    Grammar::new(
        &[Tk(Let), Identifier, Expr(let_expr)],
        Some(ifelse_expr),
        |mut result, token_stream| {
            if result.succeed {
                // Build LET expression

                //TODO: convertim "result.atoms" en un tipus (AtomList) de manera que puguem fer un pop_expr(), pop_ident(), etc, sense haver de fer pop, as_xyz, unwrap, etc.

                let let_expr = result.atoms.pop().unwrap().as_expr().unwrap();
                let let_ident = result.atoms.pop().unwrap().as_ident().unwrap();
                let pos = result.atoms.pop().unwrap().pos();

                let expr = Expr::new(
                    ExprSet::Let {
                        name: let_ident,
                        value: Box::new(let_expr),
                    },
                    pos,
                );

                Ok(expr)
            } else {
                if result.atoms.is_empty() {
                    // Not a LET expr, keep parsing in precedence order
                    todo!()
                } else {
                    // Error parsing LET expression
                    Err(result
                        .error
                        .expect("Result error can't be None if succeed is false"))
                }
            }
        },
    )
}

// "if" "(" expr ")" expr "else" expr
fn ifelse_expr() -> Grammar {
    Grammar::new(
        &[
            Tk(If),
            Tk(OpenParenth),
            Expr(expr),
            Tk(ClosingParenth),
            Expr(expr),
            Tk(Else),
            Expr(expr),
        ],
        Some(equality_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "!=" | "==" ) next_expr )+
fn equality_expr() -> Grammar {
    Grammar::new(
        &[
            Expr(comparison_expr),
            Plu(&[
                OrTk(&[TokenKind::NotEqual, TokenKind::TwoEquals]),
                Expr(comparison_expr),
            ]),
        ],
        Some(comparison_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( ">" | "<" | "<=" | ">=" | "&&" | "||" ) next_expr )+
fn comparison_expr() -> Grammar {
    Grammar::new(
        &[
            Expr(logic_expr),
            Plu(&[
                OrTk(&[
                    TokenKind::GreaterThan,
                    TokenKind::LesserThan,
                    TokenKind::GtEqual,
                    TokenKind::LtEqual,
                    TokenKind::TwoAnds,
                    TokenKind::TwoOrs,
                ]),
                Expr(logic_expr),
            ]),
        ],
        Some(logic_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "&" | "|" ) next_expr )+
fn logic_expr() -> Grammar {
    Grammar::new(
        &[
            Expr(term_expr),
            Plu(&[OrTk(&[TokenKind::And, TokenKind::Or]), Expr(term_expr)]),
        ],
        Some(term_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "+" | "-" ) next_expr )+
fn term_expr() -> Grammar {
    Grammar::new(
        &[
            Expr(factor_expr),
            Plu(&[
                OrTk(&[TokenKind::Plus, TokenKind::Minus]),
                Expr(factor_expr),
            ]),
        ],
        Some(factor_expr),
        |mut result, token_stream| todo!(),
    )
}

// next_expr ( ( "*" | "/" | "%" ) next_expr )+
fn factor_expr() -> Grammar {
    Grammar::new(
        &[
            Expr(unary_expr),
            Plu(&[
                OrTk(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]),
                Expr(unary_expr),
            ]),
        ],
        Some(unary_expr),
        |mut result, token_stream| todo!(),
    )
}

// ( "!" | "-" )+ next_expr
fn unary_expr() -> Grammar {
    Grammar::new(
        &[
            Plu(&[OrTk(&[TokenKind::Minus, TokenKind::Not])]),
            Expr(group_expr),
        ],
        Some(group_expr),
        |mut result, token_stream| todo!(),
    )
}

// "(" expr ")"
fn group_expr() -> Grammar {
    Grammar::new(
        &[Tk(OpenParenth), Expr(expr), Tk(ClosingParenth)],
        Some(primary_expr),
        |mut result, token_stream| todo!(),
    )
}

// LITERAL | ID | type_expr
fn primary_expr() -> Grammar {
    Grammar::new(
        &[Sel(&[
            &[Literal],
            &[Identifier],
            //TODO: type expression
        ])],
        None,
        |mut result, token_stream| {
            if result.succeed {
                // Build primary expression
                let primary_atom = result.atoms.pop().unwrap();
                let pos = primary_atom.pos();

                let expr = match primary_atom {
                    ResultAtom::Ident { pos, val } => Ok(Expr::new(ExprSet::Identifier(val), pos)),
                    ResultAtom::StrLiteral { pos, val } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::String(val)), pos))
                    }
                    ResultAtom::IntLiteral { pos, val } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::Integer(val)), pos))
                    }
                    ResultAtom::FltLiteral { pos, val } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::Float(val)), pos))
                    }
                    ResultAtom::BoolLiteral { pos, val } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::Boolean(val)), pos))
                    }
                    ResultAtom::NoneLiteral { pos } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::None), pos))
                    }
                    ResultAtom::NullLiteral { pos } => {
                        Ok(Expr::new(ExprSet::Literal(Literal::Null), pos))
                    }
                    _ => Result::ize_err("Expected a primary expression".into(), pos),
                }?;

                Ok(expr)
            } else {
                if result.atoms.is_empty() {
                    // Error
                    Result::ize_err(
                        "Couldn't parse a valid expression at end".into(),
                        token_stream.last_pos(),
                    )
                } else {
                    // Error parsing LET expression
                    Err(result.error.unwrap())
                }
            }
        },
    )
}
