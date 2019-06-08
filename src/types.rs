use std::collections::HashMap;
use std::fmt;
use std::sync::Mutex;

use crate::expr::uni::*;

#[derive(Clone)]
pub enum TypeKind {
    PrimitiveType(PrimitiveType),
    Function(Vec<Box<TypeKind>>, Box<TypeKind>),
    Custom(Id),
}

impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::PrimitiveType(primitive) => write!(f, "{:?}", primitive),
            TypeKind::Function(args, ret_type) => {
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
            TypeKind::Function(left_args, left_ret) => {
                if let TypeKind::Function(right_args, right_ret) = other {
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
pub enum PrimitiveType {
    Int,
    String,
    Boolean,
    Array(Box<PrimitiveType>),
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
            PrimitiveType::Array(left) => {
                if let PrimitiveType::Array(right) = other {
                    left == right
                } else {
                    false
                }
            }
        }
    }
}
impl Eq for PrimitiveType {}

lazy_static! {
    pub static ref TYPE_MAP: Mutex<HashMap<String, Uni>> = {
        let mut m = HashMap::new();
        Mutex::new(m)
    };
}
