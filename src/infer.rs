use combine::Parser;
use std::collections::HashMap;

use crate::error::*;
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

    pub fn try_insert(&mut self, id: Id, new_type: TypeResult) -> Result<TypeResult, String> {
        if let Some(defined_type) = self.try_get(&id) {
            match (defined_type, &new_type) {
                (TypeResult::Resolved(ref defined), TypeResult::Resolved(ref new)) => {
                    if defined == new {
                        Ok(new_type)
                    } else {
                        Err(create_type_mismatch_err(defined, &new))
                    }
                }
                // TODO: need to confirmed
                _ => self
                    .0
                    .insert(id, new_type)
                    .ok_or(String::from("should not come here")),
            }
        } else {
            self.0
                .insert(id, new_type)
                .ok_or(String::from("should not come here"))
        }
    }

    pub fn try_get(&mut self, id: &Id) -> Option<&mut TypeResult> {
        self.0.get_mut(id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeResult {
    Resolved(TypeKind),
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Unknown(Id),
    Err(String),
}

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
            Statement::Assign(Assign(id, expr)) => resolve_assign(id, expr, &mut type_map),
            _ => unimplemented!(),
        };

        if let TypeResult::Err(err_str) = result {
            return Err(err_str);
        }
    }
    Ok(())
}

pub fn resolve_assign(id: Id, exp: Expr, type_map: &mut TypeMap) -> TypeResult {
    let right_type = resolve_expr(exp, type_map);
    if let Some(left_type) = type_map.try_get(&id) {
        match left_type {
            TypeResult::Resolved(left_type) => {
                if let Some(err_str) = validate_assign_type(left_type, &right_type) {
                    return TypeResult::Err(err_str);
                }
            }
            TypeResult::Err(_) => return left_type.clone(),
            _ => {}
        };
    } else {
        return TypeResult::Err(create_not_initialized_err(&id));
    };

    type_map.insert(id, right_type).unwrap()
}

pub fn validate_assign_type(
    left_type: &TypeKind,
    right_type_result: &TypeResult,
) -> Option<String> {
    match right_type_result {
        TypeResult::Resolved(right_type) => {
            if *left_type != *right_type {
                Some(create_type_mismatch_err(left_type, right_type))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn resolve_expr(exp: Expr, type_map: &mut TypeMap) -> TypeResult {
    match exp {
        Expr::Unary(uni) => resolve_type(uni, type_map),
        Expr::Binary(left, op, right) => resolve_binary(*left, op, *right, type_map),
        Expr::Call(ids, _) => unreachable!(),
        Expr::Fn(_, _, _) => unreachable!(),
    }
}

pub fn resolve_binary(
    left: Expr,
    op: BinOpKind,
    right: Expr,
    type_map: &mut TypeMap,
) -> TypeResult {
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

pub fn resolve_type(uni: Uni, type_map: &mut TypeMap) -> TypeResult {
    match uni {
        Uni::Id(id) => match type_map.try_get(&id) {
            Some(result @ TypeResult::Resolved(_)) => result.clone(),
            _ => TypeResult::Unknown(id),
        },
        Uni::String(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
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
    type_map: &mut TypeMap,
) -> TypeResult {
    match (&left, &right) {
        (TypeResult::Resolved(ref left), TypeResult::Resolved(ref right)) => {
            if (left == right) {
                match resolve_op(&left, op, &right, type_map) {
                    Ok(_) => TypeResult::Resolved(left.clone()),
                    Err(err_str) => TypeResult::Err(err_str),
                }
            } else {
                TypeResult::Err(create_type_mismatch_err(&left, &right))
            }
        }
        (TypeResult::Unknown(id), TypeResult::Resolved(right_type)) => {
            type_map
                .try_insert(id.clone(), right.clone())
                .map_err(|err_str| return TypeResult::Err(err_str));
            match resolve_op_one_side(&right_type, op, type_map) {
                Ok(_) => TypeResult::Resolved(right_type.clone()),
                Err(err_str) => TypeResult::Err(err_str),
            }
        }
        (TypeResult::Resolved(left_type), TypeResult::Unknown(id)) => {
            type_map
                .try_insert(id.clone(), left.clone())
                .map_err(|err_str| return TypeResult::Err(err_str));
            match resolve_op_one_side(&left_type, op, type_map) {
                Ok(_) => TypeResult::Resolved(left_type.clone()),
                Err(err_str) => TypeResult::Err(err_str),
            }
        }
        (TypeResult::Unknown(_), TypeResult::Unknown(_)) => unimplemented!(),
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
        TypeKind::PrimitiveType(PrimitiveType::Boolean) => match op {
            BinOpKind::Eq | BinOpKind::Ne => Ok(()),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        TypeKind::PrimitiveType(PrimitiveType::Int) => Ok(()),
        TypeKind::PrimitiveType(PrimitiveType::String) => match op {
            BinOpKind::Add => Ok(()),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        _ => panic!(
            "resolve_op: should not come here. left:{:?} op:{:?} right:{:?}",
            left, op, right
        ),
    }
}

pub fn resolve_op_one_side(
    oneside: &TypeKind,
    op: BinOpKind,
    type_map: &TypeMap,
) -> Result<(), String> {
    match oneside {
        TypeKind::PrimitiveType(PrimitiveType::Boolean) => match op {
            BinOpKind::Eq | BinOpKind::Ne => Ok(()),
            _ => Err(create_cannot_use_op_one_side_err(oneside, op)),
        },
        TypeKind::PrimitiveType(PrimitiveType::Int) => Ok(()),
        TypeKind::PrimitiveType(PrimitiveType::String) => match op {
            BinOpKind::Add => Ok(()),
            _ => Err(create_cannot_use_op_one_side_err(oneside, op)),
        },
        _ => panic!(
            "resolve_op: should not come here. oneside:{:?} op:{:?}",
            oneside, op
        ),
    }
}

mod test {
    use crate::ast::*;
    use crate::expr::bin_op::*;
    use crate::infer::*;
    use crate::statement::*;

    macro_rules! assert_infer {
        ($input: expr, $expected: expr) => {
            let mut type_map = TypeMap::new();
            if let Ok((statements, _)) = ast().easy_parse($input) {
                assert_eq!(infer(statements, &mut type_map), $expected);
            } else {
                panic!("should not come here");
            }
        };
    }

    #[test]
    fn let_type_correct() {
        let input = r#"let abc = 123 in (
          abc + 456;
        )"#;
        assert_infer!(input, Ok(()));
    }

    #[test]
    fn let_assign_uninitialized() {
        let input = r#"let abc = 123 in (
          def = 456;
        )"#;
        assert_infer!(
            input,
            Err(create_not_initialized_err(&Id(String::from("def"))))
        );
    }

    #[test]
    fn let_assign_type_mismatch() {
        let input = r#"let abc = 123 in (
          abc = 456;
          abc = "err";
        )"#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn binary_type_mismatch() {
        let input = r#"123 + "abc""#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn binary_op_mismatch() {
        let input = r#""def" - "abc""#;
        assert_infer!(
            input,
            Err(create_cannot_use_op_err(
                &TypeKind::PrimitiveType(PrimitiveType::String),
                BinOpKind::Sub,
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn type_map_overwrite() {
        let mut type_map = TypeMap::new();
        type_map.insert(
            Id(String::from("abc")),
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
        );
        type_map.insert(
            Id(String::from("abc")),
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        );
        assert_eq!(
            type_map.try_get(&Id(String::from("abc"))).unwrap().clone(),
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int))
        );
    }
}
