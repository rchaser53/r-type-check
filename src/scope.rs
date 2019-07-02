use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::types::*;
use crate::utils::*;

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum IdType {
    Object(ObjectId),
    Local(ScopeId),
}
impl Eq for IdType {}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct ObjectId(pub Id);
impl Eq for ObjectId {}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct ScopeId(pub Id);
impl Eq for ScopeId {}

#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Object(ObjectScope),
    Local(LocalScope),
}

/// scope_map in ObjectScope has only ObjectScope
/// below case is id is None
/// {
///   abc: "def"
/// };
///
#[derive(Clone, Debug)]
pub struct ObjectScope {
    pub parent_id: Option<IdType>,
    pub id: Option<ObjectId>,
    pub scope_map: RefCell<HashMap<ObjectId, Box<ObjectScope>>>,
    pub type_map: RefCell<TypeMap>,
    pub function_map: RefCell<HashMap<Id, Function>>,
}

impl ObjectScope {
    pub fn new(parent_id: Option<IdType>, id: Option<ObjectId>) -> Self {
        ObjectScope {
            parent_id,
            id,
            scope_map: RefCell::new(HashMap::new()),
            type_map: RefCell::new(TypeMap::new()),
            function_map: RefCell::new(HashMap::new()),
        }
    }
}

// TBD: need to think more
impl PartialEq for ObjectScope {
    fn eq(&self, other: &Self) -> bool {
        self.type_map == other.type_map
            && self.function_map == other.function_map
            && self.scope_map == other.scope_map
    }
}

/// scope_map in LocalScope has both LocalScope and ObjectScope
#[derive(Clone, Debug)]
pub struct LocalScope {
    pub parent_id: Option<ScopeId>,
    pub id: ObjectId,
    pub scope_map: RefCell<HashMap<IdType, Box<Scope>>>,
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

// TBD: need to think more
impl PartialEq for LocalScope {
    fn eq(&self, other: &Self) -> bool {
        self.type_map == other.type_map
            && self.function_map == other.function_map
            && self.scope_map == other.scope_map
    }
}

#[derive(Clone, PartialEq)]
pub struct TypeMap(HashMap<Id, TypeResult>);
impl TypeMap {
    pub fn new() -> Self {
        TypeMap(TypeMap::create_type_map())
    }

    fn create_type_map() -> HashMap<Id, TypeResult> {
        let mut hash_map = HashMap::new();
        hash_map.insert(
            Id(String::from("println")),
            TypeResult::Resolved(TypeKind::Function(
                Id(String::from("println")),
                vec![OpeaqueType::Unknown],
                OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(PrimitiveType::Void))),
            )),
        );
        hash_map
    }

    pub fn insert(&mut self, id: Id, value: TypeResult) -> Option<TypeResult> {
        self.0.insert(id, value)
    }

    pub fn try_insert(&mut self, id: Id, new_type: TypeResult) -> Result<TypeResult, TypeError> {
        if let Some(defined_type) = self.try_get(&id) {
            match (defined_type, &new_type) {
                (TypeResult::Resolved(ref defined), TypeResult::Resolved(ref new)) => {
                    if defined == new {
                        Ok(self.0.insert(id, new_type.clone()).unwrap())
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

impl fmt::Debug for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut hash_map = HashMap::new();
        for (key, value) in self.0.iter() {
            if !BUILT_IN_IDS.contains(key) {
                hash_map.insert(key, value);
            }
        }
        write!(f, "{:?}", hash_map)
    }
}

#[derive(Clone, Debug)]
pub enum TypeResult {
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Resolved(TypeKind),
    IdOnly(Id),
    Unknown,
}

impl TypeResult {
    pub fn extract_type_kind(&self) -> Option<TypeKind> {
        match self {
            TypeResult::Resolved(type_kind) => Some(type_kind.clone()),
            _ => None,
        }
    }
}

impl PartialEq for TypeResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeResult::Resolved(left), TypeResult::Resolved(right)) => left == right,
            _ => true,
        }
    }
}
impl Eq for TypeResult {}
