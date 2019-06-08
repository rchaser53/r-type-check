use combine::Parser;
use std::collections::HashMap;

use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

pub struct TypeMap(HashMap<Id, TypeResult>);
impl TypeMap {
    pub fn new() -> Self {
        TypeMap(HashMap::new())
    }

    pub fn insert(&mut self, id: Id, value: TypeResult) -> Option<TypeResult> {
        self.0.insert(id, value)
    }

    pub fn try_get(&self, id: &Id) -> Option<&TypeResult> {
        self.0.get(id)
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeResult {
    Resolved(TypeKind),
    Uni(UniType),
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Err(String),
}

#[derive(Debug, PartialEq)]
pub struct UniType(Id, TypeKind);

pub fn infer(statements: Vec<Statement>, mut type_map: &mut TypeMap) -> Result<(), String> {
    for statement in statements {
        let result = match statement {
            Statement::Let(id, exp, bodys) => {
                let right_type = resolve_expr(exp, &mut type_map);
                type_map.insert(id.clone(), right_type);
                let unboxed_bodys = bodys.into_iter().map(|statement| *statement).collect();
                return infer(unboxed_bodys, type_map);
            }
            Statement::Expr(expr) => resolve_expr(expr, &mut type_map),
            _ => unimplemented!(),
        };

        if let TypeResult::Err(err_str) = result {
            return Err(err_str);
        }
    }
    Ok(())
}

pub fn resolve_expr(exp: Expr, type_map: &TypeMap) -> TypeResult {
    match exp {
        Expr::Unary(uni) => resolve_type(uni, type_map),
        Expr::Binary(left, op, right) => resolve_binary(*left, op, *right, type_map),
        Expr::Call(ids, _) => unreachable!(),
        Expr::Fn(_, _, _) => unreachable!(),
    }
}

pub fn resolve_binary(left: Expr, op: BinOpKind, right: Expr, type_map: &TypeMap) -> TypeResult {
    match (left, right) {
        (Expr::Binary(l_left, l_op, l_right), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, type_map);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Binary(l_left, l_op, l_right), Expr::Unary(right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, type_map);
            let r_resolved = resolve_type(right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Unary(left), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_type(left, type_map);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Unary(left), Expr::Unary(right)) => {
            let l_resolved = resolve_type(left, type_map);
            let r_resolved = resolve_type(right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        _ => unimplemented!(),
    }
}

pub fn resolve_type(uni: Uni, type_map: &TypeMap) -> TypeResult {
    match uni {
        Uni::Id(id) => TypeResult::Uni(UniType(id.clone(), TypeKind::Undefined(vec![id]))),
        Uni::String(_) => TypeResult::Resolved(TypeKind::String),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::Int),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::Boolean),
        Uni::Field(fields) => unimplemented!(),
        Uni::Array(_) => unimplemented!(),
        Uni::HashMap(_) => unimplemented!(),
        Uni::Null => unimplemented!(),
    }
}

pub fn resolve_type_result_with_op(
    left: TypeResult,
    op: BinOpKind,
    right: TypeResult,
    type_map: &TypeMap,
) -> TypeResult {
    match (left, right) {
        (TypeResult::Resolved(left), TypeResult::Resolved(right)) => {
            if (left == right) {
                match resolve_op(&left, op, &right, type_map) {
                    Ok(_) => TypeResult::Resolved(left),
                    Err(err_str) => TypeResult::Err(err_str),
                }
            } else {
                TypeResult::Err(create_type_mismatch_err(&left, &right))
            }
        }
        (TypeResult::Uni(_), TypeResult::Resolved(_)) => unimplemented!(),
        (TypeResult::Resolved(_), TypeResult::Uni(_)) => unimplemented!(),
        (TypeResult::Uni(_), TypeResult::Uni(_)) => unimplemented!(),
        _ => unimplemented!(),
    }
}

pub fn resolve_op(
    left: &TypeKind,
    op: BinOpKind,
    right: &TypeKind,
    type_map: &TypeMap,
) -> Result<(), String> {
    match left {
        TypeKind::Boolean => match op {
            BinOpKind::Eq | BinOpKind::Ne => Ok(()),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        TypeKind::Int => Ok(()),
        TypeKind::String => match op {
            BinOpKind::Add => Ok(()),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        _ => panic!(
            "resolve_op: should not come here. left:{:?} op:{:?} right:{:?}",
            left, op, right
        ),
    }
}

fn create_type_mismatch_err(left: &TypeKind, right: &TypeKind) -> String {
    format!("type is mismatch: left:{:?} right:{:?}", left, right)
}

fn create_cannot_use_op_err(left: &TypeKind, op: BinOpKind, right: &TypeKind) -> String {
    format!(
        "cannot use op: left:{:?} op:{:?} right:{:?}",
        left, op, right
    )
}

mod test {
    use crate::ast::*;
    use crate::expr::bin_op::*;
    use crate::infer::*;
    use crate::statement::*;

    #[test]
    fn binary_type_mismatch() {
        let input = r#"123 + "abc""#;
        let mut type_map = TypeMap::new();
        if let Ok((statements, _)) = ast().easy_parse(input) {
            assert_eq!(
                infer(statements, &mut type_map),
                Err(create_type_mismatch_err(&TypeKind::Int, &TypeKind::String))
            );
        } else {
            panic!("should not come here");
        }
    }

    #[test]
    fn binary_op_mismatch() {
        let input = r#""def" - "abc""#;
        let mut type_map = TypeMap::new();
        if let Ok((statements, _)) = ast().easy_parse(input) {
            assert_eq!(
                infer(statements, &mut type_map),
                Err(create_cannot_use_op_err(
                    &TypeKind::String,
                    BinOpKind::Sub,
                    &TypeKind::String
                ))
            );
        } else {
            panic!("should not come here");
        }
    }

    #[test]
    fn type_map_overwrite() {
        let mut type_map = TypeMap::new();
        type_map.insert(
            Id(String::from("abc")),
            TypeResult::Uni(UniType(Id(String::from("abc")), TypeKind::Boolean)),
        );
        type_map.insert(
            Id(String::from("abc")),
            TypeResult::Uni(UniType(Id(String::from("abc")), TypeKind::Int)),
        );
        assert_eq!(
            type_map.try_get(&Id(String::from("abc"))).unwrap(),
            &TypeResult::Uni(UniType(Id(String::from("abc")), TypeKind::Int))
        );
    }
}
