use std::fmt;

use crate::expr::uni::*;

#[derive(Clone)]
pub enum TypeKind {
    PrimitiveType(PrimitiveType),
    Function(Id, Vec<OpeaqueType>, OpeaqueType),
    Custom(Id),
}

impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::PrimitiveType(primitive) => write!(f, "{:?}", primitive),
            TypeKind::Function(_, args, ret_type) => {
                write!(f, "args:{:?} return:{:?}", args, ret_type)
            }
            TypeKind::Custom(id) => write!(f, "custom id:{:?}", id),
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
            TypeKind::Function(_, left_args, left_ret) => {
                if let TypeKind::Function(_, right_args, right_ret) = other {
                    left_args == right_args && left_ret == right_ret
                } else {
                    false
                }
            }
            TypeKind::Custom(left) => {
                if let TypeKind::Custom(right) = other {
                    left == right
                } else {
                    false
                }
            }
        }
    }
}
impl Eq for TypeKind {}

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
