//! # Symbol Table

use crate::{
    ast::{Expression, ExpressionKind, Primary},
    err::IzeErr,
    pos::RangePos,
    FxHashMap,
};
use alloc::{string::String, vec::Vec};
use core::hash::Hash;

#[derive(Debug, Default)]
/// Symbol Table, associate each identifier in the code with a [Symbol] object.
pub struct SymbolTable {
    symbols: FxHashMap<String, Symbol>,
}

impl SymbolTable {
    /// Check if identifier is present in the ST and and is a model.
    pub fn contains_model(&self, id: &str) -> bool {
        if self.symbols.contains_key(id) {
            matches!(
                self.symbols[id].metadata,
                SymbolData::Model(_) | SymbolData::Imported(_)
            )
        } else {
            false
        }
    }

    /// Insert symbol in the ST.
    pub fn insert(&mut self, key: String, value: Symbol, pos: RangePos) -> Result<(), IzeErr> {
        if !self.symbols.contains_key(key.as_str()) {
            self.symbols.insert(key, value);
            Ok(())
        } else {
            Err(IzeErr::new(format!("Symbol already exists: {}", key), pos))
        }
    }

    /// Update entry in the ST.
    pub fn update(&mut self, key: &str, metadata: SymbolData, pos: RangePos) -> Result<(), IzeErr> {
        if self.symbols.contains_key(key) {
            let (k, mut v) = self.symbols.remove_entry(key).expect("msg");
            v.metadata = metadata;
            self.symbols.insert(k, v);
            Ok(())
        } else {
            IzeErr::err(format!("Symbol not found: {}", key), pos)
        }
    }
}

#[derive(Debug)]
/// Symbol.
pub struct Symbol {
    /// Symbol metadata.
    pub metadata: SymbolData,
}

impl Symbol {
    /// New defined (not imported) Symbol.
    pub fn new_def(metadata: SymbolData) -> Self {
        Self { metadata }
    }

    /// New imported Symbol.
    pub fn new_imported(real_sym: String) -> Self {
        Self {
            metadata: SymbolData::Imported(ImportedMetadata { real_sym }),
        }
    }

    /// Default model.
    pub fn default_model() -> Self {
        Self {
            metadata: SymbolData::Model(Default::default()),
        }
    }

    /// Default transfer.
    pub fn default_transfer() -> Self {
        Self {
            metadata: SymbolData::Transfer(Default::default()),
        }
    }

    /// Default const.
    pub fn default_const() -> Self {
        Self {
            metadata: SymbolData::Const(Default::default()),
        }
    }

    /// Default pipe.
    pub fn default_pipe() -> Self {
        Self {
            metadata: SymbolData::Pipe(Default::default()),
        }
    }
}

#[derive(Debug)]
/// Symbol metadata.
pub enum SymbolData {
    Model(ModelMetadata),
    Transfer(TransferMetadata),
    Const(ConstMetadata),
    Pipe(PipeMetadata),
    Imported(ImportedMetadata),
}

impl SymbolData {
    /// New constant symbol metadata.
    pub fn new_const(const_type: Type) -> Self {
        Self::Const(ConstMetadata::Metadata { const_type })
    }

    /// New newtype model symbol metadata.
    pub fn new_newtype_model(model_type: Type) -> Self {
        Self::Model(ModelMetadata::Metadata(ModelDataKind::Newtype(model_type)))
    }

    /// New struct model symbol metadata.
    pub fn new_struct_model(attributes: FxHashMap<String, (Option<String>, Type)>) -> Self {
        Self::Model(ModelMetadata::Metadata(ModelDataKind::Struct {
            attributes,
        }))
    }

    /// New transfer symbol metadata.
    pub fn new_traf(params: FxHashMap<String, Type>, ret: Type) -> Self {
        Self::Transfer(TransferMetadata::Metadata { params, ret })
    }
}

/// Model metadata.
#[derive(Debug, Default)]
pub enum ModelMetadata {
    #[default]
    Uninit,
    Metadata(ModelDataKind),
}

/// Model metadata kind.
#[derive(Debug)]
pub enum ModelDataKind {
    Newtype(Type),
    Struct {
        /// Key = attribute name , Value = (alias, type)
        attributes: FxHashMap<String, (Option<String>, Type)>,
    },
}

/// Transfer metadata.
#[derive(Debug, Default)]
pub enum TransferMetadata {
    #[default]
    Uninit,
    Metadata {
        /// Parameters. Key = name , Value = type.
        params: FxHashMap<String, Type>,
        /// Return type.
        ret: Type,
    },
}

/// Const metadata.
#[derive(Debug, Default)]
pub enum ConstMetadata {
    #[default]
    Uninit,
    Metadata {
        const_type: Type,
    },
}

/// Pipe metadata.
#[derive(Debug, Default)]
pub enum PipeMetadata {
    #[default]
    Uninit,
    Metadata {
        //TODO: add metadata
    },
}

/// Imported symbol metadata.
#[derive(Debug, Default)]
pub struct ImportedMetadata {
    /// Real symbol name, if not renamed will be the same that apears in the symbol table.
    pub real_sym: String,
    //TODO: add ref to original AST and ST
}

//TODO: Implement custom PartialEq and Hash to ensure that Mux[Int,Str] == Mux[Str,Int]
#[derive(Debug, PartialEq, Eq, Hash)]
/// Type representation.
pub struct Type {
    pub ident: String,
    pub subtypes: Vec<Type>,
}

impl Type {
    /// New Type.
    pub fn new(ident: String, subtypes: Vec<Type>) -> Self {
        Type { ident, subtypes }
    }
}

impl TryFrom<&Expression> for Type {
    type Error = IzeErr;

    fn try_from(value: &Expression) -> Result<Self, Self::Error> {
        match &value.kind {
            ExpressionKind::Type { ident, subtypes } => {
                let subtypes: Result<Vec<Type>, _> =
                    subtypes.iter().map(|expr| expr.try_into()).collect();
                Ok(Self::new(ident.id.clone(), subtypes?))
            }
            ExpressionKind::Primary(Primary::Identifier(id)) => Ok(Type::new(id.clone(), vec![])),
            _ => Err(IzeErr::new("Expected a type".into(), value.pos)),
        }
    }
}

//NOTE: this implementation doesn't work. Maybe implement PartialOrd to sort the array of subtypes and then hash/compare it.

// // Custom PartialEq: in the case of a Mux, types can be in different orders: Mux[Int,Str] == Mux[Str,Int]
// impl PartialEq for Type {
//     fn eq(&self, other: &Self) -> bool {
//         if self.ident == other.ident {
//             let mut self_subtype_set = FxHashSet::default();
//             for subtype in &self.subtypes {
//                 self_subtype_set.insert(subtype);
//             }
//             let mut other_subtype_set = FxHashSet::default();
//             for subtype in &other.subtypes {
//                 other_subtype_set.insert(subtype);
//             }
//             self.subtypes == other.subtypes
//         } else {
//             false
//         }
//     }
// }

// // Manually implement hash to assert that: IF k1 == k2 THEN hash(k1) == hash(k2)
// impl Hash for Type {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.ident.hash(state);
//     }
// }

pub const STR_TYPE: &str = "Str";
pub const INT_TYPE: &str = "Int";
pub const FLOAT_TYPE: &str = "Float";
pub const BOOL_TYPE: &str = "Bool";
pub const NONE_TYPE: &str = "None";
pub const NULL_TYPE: &str = "Null";
pub const ANY_TYPE: &str = "Any";

pub const MAP_TYPE: &str = "Map";
pub const LIST_TYPE: &str = "List";
pub const MUX_TYPE: &str = "Mux";
pub const TUPLE_TYPE: &str = "Tuple";
pub const TRAF_TYPE: &str = "Traf";
pub const REST_MARKER: &str = "...";
