use combine::{attempt, between, choice, many, parser, Parser};

use std::collections::HashMap;

use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

type TypeMap = HashMap<Id, TypeResult>;

#[derive(Debug, PartialEq)]
pub enum TypeResult {
    Resolved(TypeKind),
    Uni(UniType),
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Err(String),
}

#[derive(Debug, PartialEq)]
pub struct UniType(Id, TypeKind);

pub fn infer(statements: Vec<Statement>) -> Result<(), String> {
    for statement in statements {
        let mut type_map: TypeMap = HashMap::new();
        let result = match statement {
            Statement::Let(id, exp, bodys) => {
                let right_type = resolve_expr(exp);
                let result = type_map.insert(id.clone(), right_type);
                result.unwrap()
            }
            Statement::Expr(expr) => resolve_expr(expr),
            _ => unimplemented!(),
        };

        if let TypeResult::Err(err_str) = result {
            return Err(err_str);
        }
    }
    Ok(())
}

pub fn resolve_expr(exp: Expr) -> TypeResult {
    match exp {
        Expr::Unary(uni) => resolve_type(uni),
        Expr::Binary(left, op, right) => resolve_binary(*left, op, *right),
        Expr::Call(ids, _) => unreachable!(),
        Expr::Fn(_, _, _) => unreachable!(),
    }
}

pub fn resolve_binary(left: Expr, op: BinOpKind, right: Expr) -> TypeResult {
    match (left, right) {
        (Expr::Binary(l_left, l_op, l_right), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right);
            resolve_type_result_with_op(l_resolved, op, r_resolved)
        }
        (Expr::Binary(l_left, l_op, l_right), Expr::Unary(right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right);
            let r_resolved = resolve_type(right);
            resolve_type_result_with_op(l_resolved, op, r_resolved)
        }
        (Expr::Unary(left), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_type(left);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right);
            resolve_type_result_with_op(l_resolved, op, r_resolved)
        }
        (Expr::Unary(left), Expr::Unary(right)) => {
            let l_resolved = resolve_type(left);
            let r_resolved = resolve_type(right);
            resolve_type_result_with_op(l_resolved, op, r_resolved)
        }
        _ => unimplemented!(),
    }
}

pub fn resolve_type(uni: Uni) -> TypeResult {
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
) -> TypeResult {
    match (left, right) {
        (TypeResult::Resolved(left), TypeResult::Resolved(right)) => {
            if (left == right) {
                match resolve_op(&left, op, &right) {
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

pub fn resolve_op(left: &TypeKind, op: BinOpKind, right: &TypeKind) -> Result<(), String> {
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
        if let Ok((statements, _)) = ast().easy_parse(input) {
            assert_eq!(
                infer(statements),
                Err(create_type_mismatch_err(&TypeKind::Int, &TypeKind::String))
            );
        } else {
            panic!("should not come here");
        }
    }

    #[test]
    fn binary_op_mismatch() {
        let input = r#""def" - "abc""#;
        if let Ok((statements, _)) = ast().easy_parse(input) {
            assert_eq!(
                infer(statements),
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
}
