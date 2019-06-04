use std::collections::HashMap;

use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

pub fn infer(statements: Vec<Statement>) {
    for statement in statements {
        match statement {
            Statement::Let(id, exp, bodys) => {
                let mut type_map: HashMap<Id, TypeKind> = HashMap::new();

                let right_type = match exp {
                    Expr::Unary(uni) => resolve_type(uni),
                    Expr::Binary(left, op, right) => unimplemented!(),
                    Expr::Call(ids, _) => TypeKind::Undefined(ids),
                    Expr::Fn(_, _, _) => unreachable!(),
                };

                type_map.insert(id.clone(), right_type);
            }
            _ => unimplemented!(),
        }
    }
}

pub fn resolve_type(uni: Uni) -> TypeKind {
    match uni {
        Uni::Id(id) => TypeKind::Undefined(vec![id]),
        Uni::Field(fields) => TypeKind::Undefined(fields),
        Uni::String(_) => TypeKind::String,
        Uni::Number(_) => TypeKind::Int,
        Uni::Boolean(_) => TypeKind::Boolean,
        Uni::Array(_) => unimplemented!(),
        Uni::HashMap(_) => unimplemented!(),
        Uni::Null => unimplemented!(),
    }
}
