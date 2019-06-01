use std::collections::HashMap;
use std::sync::Mutex;

use crate::expr::uni::*;
use crate::expr::*;

#[derive(Debug, PartialEq)]
pub enum _TypeKind {
    Int,
    String,
    Boolean,
}

lazy_static! {
    pub static ref TYPE_MAP: Mutex<HashMap<String, Uni>> = {
        let mut m = HashMap::new();
        Mutex::new(m)
    };
}
