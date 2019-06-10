use combine::Parser;
use std::collections::HashMap;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

#[derive(Clone)]
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
                        // TODO: Maybe cannot come here now
                        Err(create_infered_other_type_err(&id, defined, &new))
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
    Binary(Box<TypeResult>, BinOpKind, Box<TypeResult>),
    Resolved(TypeKind),
    IdOnly(Id),
    Unknown,
    Err(String),
}

/// return function return TypeResult
pub fn infer(statements: Vec<Statement>, mut type_map: &mut TypeMap) -> Result<TypeResult, String> {
    let mut return_type_results = vec![];
    for statement in statements {
        match statement {
            Statement::Let(id, exp, body) => {
                let right_type = resolve_expr(exp, &mut type_map);
                if let TypeResult::Err(err_str) = right_type {
                    return Err(err_str);
                }
                type_map.insert(id.clone(), right_type);
                let unboxed_body = body.into_iter().map(|statement| *statement).collect();
                infer(unboxed_body, type_map)?;
            }
            Statement::Expr(expr) => {
                let type_result = resolve_expr(expr, &mut type_map);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
            }
            Statement::Assign(Assign(id, expr)) => {
                let type_result = resolve_assign(id, expr, &mut type_map);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
            }
            Statement::Return(expr) => {
                let type_result = resolve_expr(expr, &mut type_map);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
                return_type_results.push(type_result.clone());
            }
            Statement::If(if_tuples) => {
                for (if_condition, boxed_body) in if_tuples {
                    let if_condition_type_result = resolve_expr(if_condition, &mut type_map);
                    match if_condition_type_result {
                        TypeResult::Resolved(type_kind) => {
                            if type_kind != TypeKind::PrimitiveType(PrimitiveType::Boolean) {
                                return Err(create_if_condition_not_boolean_err(&type_kind));
                            }
                        }
                        TypeResult::IdOnly(id) => {
                            type_map.insert(
                                id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(
                                    PrimitiveType::Boolean,
                                )),
                            );
                        }
                        TypeResult::Err(err_str) => return Err(err_str),
                        _ => unreachable!(),
                    };
                    let unboxed_body = boxed_body.into_iter().map(|statement| *statement).collect();
                    infer(unboxed_body, type_map)?;
                }
            }
        };
    }

    if return_type_results.is_empty() {
        Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Void,
        )))
    } else {
        let first_type_result = return_type_results.pop().unwrap();
        for return_type_result in return_type_results {
            if first_type_result != return_type_result {
                return Err(create_conflict_type_return_err(
                    &first_type_result,
                    &return_type_result,
                ));
            }
        }
        return Ok(first_type_result);
    }
}

pub fn resolve_assign(id: Id, exp: Expr, type_map: &mut TypeMap) -> TypeResult {
    let right_type = resolve_expr(exp, type_map);
    if let TypeResult::Err(err_str) = right_type {
        return TypeResult::Err(err_str.to_string());
    }

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
        Expr::Fn(args, body) => {
            let mut fn_type_map = TypeMap::new();
            for arg in args.clone() {
                fn_type_map.insert(arg.clone(), TypeResult::IdOnly(arg));
            }
            match infer(
                body.into_iter().map(|boxed| *boxed).collect(),
                &mut fn_type_map,
            ) {
                Ok(result) => {
                    let fn_arg_types = args
                        .into_iter()
                        .map(|id| {
                            if let Some(TypeResult::Resolved(type_kind)) = fn_type_map.try_get(&id)
                            {
                                OpeaqueType::Defined(Box::new(type_kind.clone()))
                            } else {
                                OpeaqueType::IdOnly(id)
                            }
                        })
                        .collect();
                    let return_type = match result {
                        TypeResult::Resolved(return_type) => {
                            OpeaqueType::Defined(Box::new(return_type))
                        }
                        TypeResult::IdOnly(id) => OpeaqueType::IdOnly(id),
                        _ => unreachable!(),
                    };
                    // TBD: need to think more
                    TypeResult::Resolved(TypeKind::Function(fn_arg_types, return_type))
                }
                Err(err_str) => TypeResult::Err(err_str),
            }
        }
        Expr::Call(ids, boxed_args) => resolve_call(ids, boxed_args, type_map),
    }
}

pub fn resolve_call(ids: Vec<Id>, args: Vec<Box<Expr>>, type_map: &mut TypeMap) -> TypeResult {
    // TBD: need to implement correctly
    // especially for field
    // ex. xx.yy();
    let id = &ids[0];
    let ret_result = match type_map.try_get(&id) {
        Some(TypeResult::IdOnly(_)) | None => {
            let arg_len = args.len();
            let mut arg_type_vec = Vec::with_capacity(arg_len);
            for index in 0..arg_len {
                arg_type_vec[index] = OpeaqueType::Unknown
            }
            type_map
                .insert(
                    id.clone(),
                    TypeResult::Resolved(TypeKind::Function(arg_type_vec, OpeaqueType::Unknown)),
                )
                .unwrap()
        }
        Some(ret_result @ _) => ret_result.clone(),
    };

    match ret_result {
        TypeResult::Resolved(TypeKind::Function(params, return_opeaque)) => {
            for (index, param) in params.into_iter().enumerate() {
                match param {
                    OpeaqueType::Defined(param_type_kind) => {
                        let arg_exp = args.get(index).unwrap();
                        let arg_type_result = resolve_expr(*arg_exp.clone(), type_map);
                        let param_type_result = TypeResult::Resolved(*param_type_kind.clone());
                        if arg_type_result != param_type_result {
                            return TypeResult::Err(create_param_and_arg_type_is_mismatch_err(
                                &arg_type_result,
                                &param_type_result,
                            ));
                        }
                    }
                    _ => {
                        // TBD: need to check correctly
                    }
                }
            }

            match return_opeaque {
                OpeaqueType::Defined(boxed_type_kind) => {
                    TypeResult::Resolved(*boxed_type_kind.clone())
                }
                OpeaqueType::IdOnly(id) => TypeResult::IdOnly(id.clone()),
                OpeaqueType::Unknown => TypeResult::Unknown,
            }
        }
        TypeResult::Resolved(type_kind @ _) => {
            TypeResult::Err(create_cannot_call_err(&id, &type_kind))
        }
        TypeResult::IdOnly(id) => TypeResult::IdOnly(id.clone()),
        _ => unreachable!(),
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
        (Expr::Binary(l_left, l_op, l_right), Expr::Call(ids, args)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, type_map);
            let r_resolved = resolve_call(ids, args, type_map);
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
        (Expr::Unary(left), Expr::Call(ids, args)) => {
            let l_resolved = resolve_type(left, type_map);
            let r_resolved = resolve_call(ids, args, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Call(ids, args), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_call(ids, args, type_map);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Call(ids, args), Expr::Unary(right)) => {
            let l_resolved = resolve_call(ids, args, type_map);
            let r_resolved = resolve_type(right, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        (Expr::Call(left_ids, left_args), Expr::Call(right_ids, right_args)) => {
            let l_resolved = resolve_call(left_ids, left_args, type_map);
            let r_resolved = resolve_call(right_ids, right_args, type_map);
            resolve_type_result_with_op(l_resolved, op, r_resolved, type_map)
        }
        _ => unreachable!(),
    }
}

pub fn resolve_type(uni: Uni, type_map: &mut TypeMap) -> TypeResult {
    match uni {
        Uni::Id(id) => match type_map.try_get(&id) {
            Some(result @ TypeResult::Resolved(_)) => result.clone(),
            _ => TypeResult::IdOnly(id),
        },
        Uni::String(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
        Uni::Field(_) => unimplemented!(),
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
            check_left_op_right(left, op, right, type_map)
        }
        (TypeResult::IdOnly(id), TypeResult::Resolved(right_type)) => {
            try_insert_and_resolve_op(&right, right_type, &id, op, type_map)
        }
        (TypeResult::Resolved(left_type), TypeResult::IdOnly(id)) => {
            try_insert_and_resolve_op(&left, left_type, &id, op, type_map)
        }
        (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => {
            let left_result = filter_type_result(&left, &left_id, type_map)
                .map_err(|err_str| return TypeResult::Err(err_str))
                .unwrap();

            let right_result = filter_type_result(&right, &right_id, type_map)
                .map_err(|err_str| return TypeResult::Err(err_str))
                .unwrap();

            match (left_result, right_result) {
                (TypeResult::Resolved(left_type), TypeResult::Resolved(_)) => {
                    TypeResult::Resolved(left_type.clone())
                }
                (TypeResult::Resolved(left_type), TypeResult::IdOnly(right_id)) => {
                    try_insert_and_resolve_op(&left_result, left_type, &right_id, op, type_map)
                }
                (TypeResult::IdOnly(left_id), TypeResult::Resolved(right_type)) => {
                    try_insert_and_resolve_op(&right_result, right_type, &left_id, op, type_map)
                }
                (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => {
                    // TBD need to implemnt correctly
                    // the below case is not covert
                    /*
                     *  fn(a, b) {
                     *    // this case return type are Int, String, Boolean, ...
                     *    return a + b;
                     *  }
                     */
                    match op {
                        BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Shr => {
                            type_map.insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            type_map.insert(
                                right_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int))
                        }
                        BinOpKind::Lt | BinOpKind::Le | BinOpKind::Ge | BinOpKind::Gt => {
                            type_map.insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            type_map.insert(
                                right_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean))
                        }
                        _ => TypeResult::IdOnly(left_id.clone()),
                    }
                }
                _ => unreachable!(),
            }
        }
        (TypeResult::Err(err_str), _) | (_, TypeResult::Err(err_str)) => {
            TypeResult::Err(err_str.to_string())
        }
        _ => unimplemented!(),
    }
}

pub fn try_insert_and_resolve_op(
    original: &TypeResult,
    original_type: &TypeKind,
    id: &Id,
    op: BinOpKind,
    type_map: &mut TypeMap,
) -> TypeResult {
    let _ = type_map
        .try_insert(id.clone(), original.clone())
        .map_err(|err_str| return TypeResult::Err(err_str));
    match resolve_op_one_side(&original_type, op, type_map) {
        Ok(_) => TypeResult::Resolved(original_type.clone()),
        Err(err_str) => TypeResult::Err(err_str),
    }
}

// return Resolved or Unknown only
pub fn filter_type_result<'a>(
    original: &'a TypeResult,
    id: &Id,
    type_map: &mut TypeMap,
) -> Result<&'a TypeResult, String> {
    match type_map.try_get(id) {
        Some(TypeResult::Resolved(_)) | Some(TypeResult::IdOnly(_)) => Ok(original),
        None => {
            type_map.insert(id.clone(), TypeResult::IdOnly(id.clone()));
            Ok(original)
        }
        Some(unknown) => unimplemented!(),
        Some(TypeResult::Err(err_str)) => Err(err_str.to_string()),
        Some(TypeResult::Binary(_, _, _)) => unreachable!(),
    }
}

pub fn check_left_op_right(
    left: &TypeKind,
    op: BinOpKind,
    right: &TypeKind,
    type_map: &mut TypeMap,
) -> TypeResult {
    if left == right {
        match resolve_op(&left, op, &right, type_map) {
            Ok(result) => result,
            Err(err_str) => TypeResult::Err(err_str),
        }
    } else {
        TypeResult::Err(create_type_mismatch_err(&left, &right))
    }
}

pub fn resolve_op(
    left: &TypeKind,
    op: BinOpKind,
    right: &TypeKind,
    _type_map: &TypeMap,
) -> Result<TypeResult, String> {
    match left {
        TypeKind::PrimitiveType(PrimitiveType::Boolean) => match op {
            BinOpKind::Eq | BinOpKind::Ne => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Boolean,
            ))),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        TypeKind::PrimitiveType(PrimitiveType::Int) => match op {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Shr => {
                Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                    PrimitiveType::Int,
                )))
            }
            _ => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Boolean,
            ))),
        },
        TypeKind::PrimitiveType(PrimitiveType::String) => match op {
            BinOpKind::Add => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::String,
            ))),
            _ => Err(create_cannot_use_op_err(left, op, right)),
        },
        _ => unreachable!(
            "resolve_op: should not come here. left:{:?} op:{:?} right:{:?}",
            left, op, right
        ),
    }
}

pub fn resolve_op_one_side(
    oneside: &TypeKind,
    op: BinOpKind,
    _type_map: &TypeMap,
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
        _ => unreachable!(
            "resolve_op_one_side: should not come here. oneside:{:?} op:{:?}",
            oneside, op
        ),
    }
}

mod test {
    use crate::ast::*;
    use crate::infer::*;

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
    fn let_set() {
        let input = r#"let abc = 123 + "abc" in (
        )"#;
        // TODO: need to improve error message
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn let_infer() {
        let input = r#"let abc = 123 in (
          abc + 456;
        )"#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"let abc = def in (
          abc + 456;
          abc + "def";
        )"#;
        // TODO: need to improve error message
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );

        let input = r#"let abc = true in (
          abc = (123 > 456) + 44;
        )"#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int)
            ))
        );

        let input = r#"let abc = def in (
          (abc + 234) != (abc == false);
        )"#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean)
            ))
        );
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
    fn fn_infer_correct() {
        let input = r#"fn (aaa, bbb, ccc) {
            123 + 345;
            aaa + 123;
            bbb + 456;
            789 + ccc;
            aaa + bbb;
            aaa + bbb + ccc;
        }"#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"fn (aaa, bbb) {
            aaa + 123;
            bbb + 456;
            aaa + bbb + "abc";
        }"#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn return_infer_incorrect() {
        let input = r#"
            return 123;
            return "abc";
        "#;
        assert_infer!(
            input,
            Err(create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            ))
        );

        let input = r#"
            fn() {
              return 123;
              return "abc";
            }
        "#;
        assert_infer!(
            input,
            Err(create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            ))
        );

        let input = r#"
            fn(abc, def) {
              abc * def;
              return abc == true;
            }
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
            ))
        );
    }

    #[test]
    fn call_infer_incorrect() {
        let input = r#"
            let test = fn(abc) {
              return abc == true;
            } in (
              test(false) * 2;
            );
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            ))
        );

        let input = r#"
            let test = fn(abc) {
              return abc == true;
            } in (
              test(123);
            );
        "#;
        assert_infer!(
            input,
            Err(create_param_and_arg_type_is_mismatch_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
            ))
        );

        let input = r#"
            fn(abc) {
              abc = 12;
              abc();
            }
        "#;
        assert_infer!(
            input,
            Err(create_cannot_call_err(
                &Id(String::from("abc")),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            ))
        );
    }

    #[test]
    fn failed() {
        let input = r#"
            fn(abc) {
              abc();
              abc = 12;
            }
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::Function(vec![], OpeaqueType::Unknown),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            ))
        );
    }

    #[test]
    fn if_infer() {
        let input = r#"
            fn(abc) {
                if (abc) {
                  return abc == true;
                }
                abc * 3;
            }
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
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
