use std::collections::HashMap;
use std::sync::Mutex;

use crate::expr::uni::*;

#[derive(Debug)]
pub enum TypeKind {
    Int,
    String,
    Boolean,
    Undefined(Vec<Id>),
}

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TypeKind::Int => {
                if let TypeKind::Int = other {
                    true
                } else {
                    false
                }
            }
            TypeKind::String => {
                if let TypeKind::String = other {
                    true
                } else {
                    false
                }
            }
            TypeKind::Boolean => {
                if let TypeKind::Boolean = other {
                    true
                } else {
                    false
                }
            }
            TypeKind::Undefined(_) => false,
        }
    }
}
impl Eq for TypeKind {}

lazy_static! {
    pub static ref TYPE_MAP: Mutex<HashMap<String, Uni>> = {
        let mut m = HashMap::new();
        Mutex::new(m)
    };
}
