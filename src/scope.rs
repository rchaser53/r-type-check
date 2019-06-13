use std::collections::HashMap;
use std::sync::Mutex;

use crate::expr::uni::*;
use crate::expr::*;
use crate::infer::*;

pub struct Pool(Mutex<i64>);
impl Pool {
    pub fn next_id(&self) -> Id {
        let mut temp = self.0.lock().unwrap();
        *temp += 1;
        Id(temp.to_string())
    }

    pub fn new() -> Pool {
        Pool(Mutex::new(0))
    }
}

lazy_static! {
    pub static ref ID_POOL: Pool = {
        let mut pool = Pool::new();
        pool
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdType {
    Object(ObjectId),
    Scope(ScopeId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectId(Id);

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeId(Id);

#[derive(Clone, Debug)]
pub enum Scope {
    Object(ObjectScope),
    Local(LocalScope),
}

#[derive(Clone, Debug)]
pub struct ObjectScope {
    pub parent_id: Option<ObjectId>,
    pub id: ObjectId,
    pub scope_map: HashMap<Id, Box<ObjectScope>>,
    pub type_map: TypeMap,
    pub function_map: HashMap<Id, Function>,
}

#[derive(Clone, Debug)]
pub struct LocalScope {
    pub parent_id: Option<ScopeId>,
    pub id: ObjectId,
    pub scope_map: HashMap<Id, Box<Scope>>,
    pub type_map: TypeMap,
    pub function_map: HashMap<Id, Function>,
}

impl LocalScope {
    pub fn new(parent_id: Option<ScopeId>) -> Self {
        LocalScope {
            parent_id,
            id: ObjectId(ID_POOL.next_id()),
            scope_map: HashMap::new(),
            type_map: TypeMap::new(),
            function_map: HashMap::new(),
        }
    }
}
