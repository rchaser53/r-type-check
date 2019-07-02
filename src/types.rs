use std::fmt;

use crate::expr::uni::*;
use crate::scope::*;

#[derive(Clone)]
pub enum TypeKind {
    PrimitiveType(PrimitiveType),
    Polymorphism(Vec<TypeKind>),
    Function(Id, Vec<OpeaqueType>, OpeaqueType),
    Scope(IdType),
}

impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::PrimitiveType(primitive) => write!(f, "{:?}", primitive),
            TypeKind::Polymorphism(primitives) => write!(f, "{:?}", primitives),
            TypeKind::Function(_, args, ret_type) => {
                write!(f, "args:{:?} return:{:?}", args, ret_type)
            }
            TypeKind::Scope(id_type) => match id_type {
                IdType::Local(local_id) => write!(f, "local id:{:?}", local_id),
                IdType::Object(object_id) => write!(f, "object id:{:?}", object_id),
            },
        }
    }
}

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TypeKind::PrimitiveType(left) => {
                if let TypeKind::PrimitiveType(right) = other {
                    left == right
                } else {
                    false
                }
            }
            TypeKind::Polymorphism(left_types) => {
                if let Some(_) = left_types.into_iter().find(|left_type| *left_type == other) {
                    true
                } else {
                    false
                }
            }
            TypeKind::Function(_, left_args, left_ret) => {
                if let TypeKind::Function(_, right_args, right_ret) = other {
                    left_args == right_args && left_ret == right_ret
                } else {
                    false
                }
            }
            TypeKind::Scope(left) => {
                if let TypeKind::Scope(right) = other {
                    left == right
                } else {
                    false
                }
            }
        }
    }
}
impl Eq for TypeKind {}

impl TypeKind {
    pub fn convert_opeaque(self) -> OpeaqueType {
        match &self {
            TypeKind::PrimitiveType(_) | TypeKind::Function(_, _, _) => {
                OpeaqueType::Defined(Box::new(self))
            }
            _ => panic!("failed to convert TypeKind::Scope {:?}", self),
        }
    }
}

#[derive(Clone, Debug)]
pub enum OpeaqueType {
    Unknown,
    IdOnly(Id),
    Defined(Box<TypeKind>),
}
impl PartialEq for OpeaqueType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            OpeaqueType::IdOnly(left) => {
                if let OpeaqueType::IdOnly(right) = other {
                    left == right
                } else {
                    false
                }
            }
            OpeaqueType::Defined(left) => {
                if let OpeaqueType::Defined(right) = other {
                    left == right
                } else {
                    false
                }
            }
            OpeaqueType::Unknown => false,
        }
    }
}
impl Eq for OpeaqueType {}

#[derive(Clone, Debug)]
pub enum PrimitiveType {
    Int,
    String,
    Boolean,
    Array(ArrayType),
    Void,
}

#[derive(Clone, Debug)]
pub enum ArrayType {
    Unknown, // for empty
    Defined(Box<PrimitiveType>),
}

impl PartialEq for PrimitiveType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            PrimitiveType::Int => {
                if let PrimitiveType::Int = other {
                    true
                } else {
                    false
                }
            }
            PrimitiveType::String => {
                if let PrimitiveType::String = other {
                    true
                } else {
                    false
                }
            }
            PrimitiveType::Boolean => {
                if let PrimitiveType::Boolean = other {
                    true
                } else {
                    false
                }
            }
            PrimitiveType::Array(ArrayType::Unknown) => {
                if let PrimitiveType::Array(_) = other {
                    true
                } else {
                    false
                }
            }
            PrimitiveType::Array(ArrayType::Defined(left)) => match other {
                PrimitiveType::Array(ArrayType::Defined(right)) => left == right,
                PrimitiveType::Array(ArrayType::Unknown) => true,
                _ => false,
            },
            PrimitiveType::Void => {
                if let PrimitiveType::Void = other {
                    true
                } else {
                    false
                }
            }
        }
    }
}
impl Eq for PrimitiveType {}
