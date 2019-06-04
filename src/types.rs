use std::collections::HashMap;
use std::sync::Mutex;

use crate::expr::uni::*;

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Int,
    String,
    Boolean,
    Undefined(Vec<Id>),
}

lazy_static! {
    pub static ref TYPE_MAP: Mutex<HashMap<String, Uni>> = {
        let mut m = HashMap::new();
        Mutex::new(m)
    };
}
