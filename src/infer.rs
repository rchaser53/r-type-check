use std::collections::HashMap;

use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

pub fn infer(statements: Vec<Statement>) -> Result<(), String> {
    for statement in statements {
        match statement {
            Statement::Let(id, exp, bodys) => {
                let mut type_map: HashMap<Id, TypeKind> = HashMap::new();
                let right_type = resolve_expr(exp);
                type_map.insert(id.clone(), right_type?);
            }
            Statement::Expr(expr) => {
                resolve_expr(expr);
            }
            _ => unimplemented!(),
        }
    }
    Ok(())
}

pub fn resolve_expr(exp: Expr) -> Result<TypeKind, String> {
    match exp {
        Expr::Unary(uni) => Ok(resolve_type(uni)),
        Expr::Binary(left, op, right) => unimplemented!(),
        Expr::Call(ids, _) => Ok(TypeKind::Undefined(ids)),
        Expr::Fn(_, _, _) => unreachable!(),
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
