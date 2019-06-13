use std::cell::RefCell;
use std::collections::HashMap;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::types::*;
use crate::utils::*;

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
    pub scope_map: RefCell<HashMap<Id, Box<ObjectScope>>>,
    pub type_map: RefCell<TypeMap>,
    pub function_map: RefCell<HashMap<Id, Function>>,
}

#[derive(Clone, Debug)]
pub struct LocalScope {
    pub parent_id: Option<ScopeId>,
    pub id: ObjectId,
    pub scope_map: RefCell<HashMap<Id, Box<Scope>>>,
    pub type_map: RefCell<TypeMap>,
    pub function_map: RefCell<HashMap<Id, Function>>,
}

impl LocalScope {
    pub fn new(parent_id: Option<ScopeId>) -> Self {
        LocalScope {
            parent_id,
            id: ObjectId(ID_POOL.next_id()),
            scope_map: RefCell::new(HashMap::new()),
            type_map: RefCell::new(TypeMap::new()),
            function_map: RefCell::new(HashMap::new()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeMap(HashMap<Id, TypeResult>);
impl TypeMap {
    pub fn new() -> Self {
        TypeMap(HashMap::new())
    }

    pub fn insert(&mut self, id: Id, value: TypeResult) -> Option<TypeResult> {
        self.0.insert(id, value)
    }

    pub fn try_insert(&mut self, id: Id, new_type: TypeResult) -> Result<TypeResult, String> {
        if let Some(defined_type) = self.try_get(&id) {
            match (defined_type, &new_type) {
                (TypeResult::Resolved(ref defined), TypeResult::Resolved(ref new)) => {
                    if defined == new {
                        Ok(new_type)
                    } else {
                        Err(create_assign_conflict_type_err(&id, defined, &new))
                    }
                }
                _ => {
                    self.0.insert(id, new_type.clone());
                    Ok(new_type)
                }
            }
        } else {
            self.0.insert(id, new_type.clone());
            Ok(new_type)
        }
    }

    pub fn try_get(&mut self, id: &Id) -> Option<&mut TypeResult> {
        self.0.get_mut(id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeResult {
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Resolved(TypeKind),
    IdOnly(Id),
    Unknown,
}

impl Eq for TypeResult {}
