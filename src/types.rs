use std::fmt;

use crate::expr::uni::*;
use crate::scope::*;

#[derive(Clone)]
pub enum TypeKind {
    PrimitiveType(PrimitiveType),
    Polymorphism(Vec<TypeKind>),
    Function(FunctionType),
    Scope(IdType),
}

#[derive(Clone, PartialEq)]
pub struct FunctionType(pub Id, pub Vec<OpeaqueType>, pub OpeaqueType);
impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "args:{:?} return:{:?}", self.1, self.2)
    }
}

impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::PrimitiveType(primitive) => write!(f, "{:?}", primitive),
            TypeKind::Polymorphism(primitives) => write!(f, "{:?}", primitives),
            TypeKind::Function(fn_type) => write!(f, "{:?}", fn_type),
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
                left_types.iter().any(|left_type| left_type == other)
            }
            TypeKind::Function(FunctionType(_, left_args, left_ret)) => {
                if let TypeKind::Function(FunctionType(_, right_args, right_ret)) = other {
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
            TypeKind::PrimitiveType(_) | TypeKind::Function(FunctionType(_, _, _)) => {
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
            OpeaqueType::Unknown => true,
        }
    }
}
impl Eq for OpeaqueType {}
impl OpeaqueType {
    pub fn convert_type_result(self) -> TypeResult {
        match self {
            OpeaqueType::Defined(boxed_type_kind) => TypeResult::Resolved(*boxed_type_kind.clone()),
            OpeaqueType::IdOnly(_) | OpeaqueType::Unknown => TypeResult::Unknown,
        }
    }
}

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
