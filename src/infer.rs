use combine::Parser;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::statement::*;
use crate::types::*;

#[derive(Clone, Debug)]
pub struct Context {
    pub type_map: RefCell<TypeMap>,
}
impl Context {
    pub fn new() -> Self {
        Context {
            type_map: RefCell::new(TypeMap::new()),
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
pub fn resolve_statement(
    statements: Vec<Statement>,
    context: &Context,
) -> Result<TypeResult, String> {
    let mut return_type_results = vec![];
    for statement in statements {
        match statement {
            Statement::Let(lets, body) => {
                for Assign(id, exp) in lets {
                    let right_type = resolve_expr(exp, &context);
                    if let TypeResult::Err(err_str) = right_type {
                        return Err(err_str);
                    }
                    context.type_map.borrow_mut().insert(id.clone(), right_type);
                }
                let unboxed_body = body.into_iter().map(|statement| *statement).collect();
                resolve_statement(unboxed_body, context)?;
            }
            Statement::Expr(expr) => {
                let type_result = resolve_expr(expr, context);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
            }
            Statement::Assign(Assign(id, expr)) => {
                let type_result = resolve_assign(id, expr, context);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
            }
            Statement::Return(expr) => {
                let type_result = resolve_expr(expr, context);
                if let TypeResult::Err(err_str) = type_result {
                    return Err(err_str);
                }
                return_type_results.push(type_result.clone());
            }
            Statement::If(if_tuples) => {
                for (if_condition, boxed_body) in if_tuples {
                    let if_condition_type_result = resolve_expr(if_condition, context);
                    match if_condition_type_result {
                        TypeResult::Resolved(type_kind) => {
                            if type_kind != TypeKind::PrimitiveType(PrimitiveType::Boolean) {
                                return Err(create_if_condition_not_boolean_err(&type_kind));
                            }
                        }
                        TypeResult::IdOnly(id) => {
                            context.type_map.borrow_mut().insert(
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

                    // get return type in if statement
                    let if_return_type_result = resolve_statement(unboxed_body, context)?;
                    return_type_results.push(if_return_type_result);
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

pub fn resolve_assign(id: Id, exp: Expr, context: &Context) -> TypeResult {
    let right_type = resolve_expr(exp, context);
    if let TypeResult::Err(err_str) = right_type {
        return TypeResult::Err(err_str.to_string());
    }

    if let Some(left_type) = context.type_map.borrow_mut().try_get(&id) {
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

    context
        .type_map
        .borrow_mut()
        .insert(id, right_type)
        .unwrap()
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

pub fn resolve_expr(exp: Expr, context: &Context) -> TypeResult {
    match exp {
        Expr::Unary(uni) => resolve_type(uni, context),
        Expr::Binary(left, op, right) => resolve_binary(*left, op, *right, context),
        Expr::Fn(args, body) => {
            let fn_context = Context::new();
            for arg in args.clone() {
                fn_context
                    .type_map
                    .borrow_mut()
                    .insert(arg.clone(), TypeResult::IdOnly(arg));
            }
            match resolve_statement(body.into_iter().map(|boxed| *boxed).collect(), &fn_context) {
                Ok(result) => {
                    let fn_arg_types = args
                        .into_iter()
                        .map(|id| {
                            if let Some(TypeResult::Resolved(type_kind)) =
                                fn_context.type_map.borrow_mut().try_get(&id)
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
        Expr::Call(ids, boxed_args) => resolve_call(ids, boxed_args, context),
    }
}

pub fn resolve_call(ids: Vec<Id>, args: Vec<Box<Expr>>, context: &Context) -> TypeResult {
    // TBD: need to implement correctly
    // especially for field
    // ex. xx.yy();
    let id = &ids[0];
    let arg_len = args.len();
    let mut arg_type_vec = Vec::with_capacity(arg_len);
    let (should_insert, ret_result) = match context.type_map.borrow_mut().try_get(&id) {
        Some(TypeResult::IdOnly(_)) | None => {
            for index in 0..arg_len {
                arg_type_vec[index] = OpeaqueType::Unknown
            }
            (
                true,
                TypeResult::Resolved(TypeKind::Function(
                    arg_type_vec.clone(),
                    OpeaqueType::Unknown,
                )),
            )
        }
        Some(result @ _) => (false, result.clone()),
    };
    if should_insert {
        context.type_map.borrow_mut().insert(
            id.clone(),
            TypeResult::Resolved(TypeKind::Function(arg_type_vec, OpeaqueType::Unknown)),
        );
    }

    match ret_result {
        TypeResult::Resolved(TypeKind::Function(params, return_opeaque)) => {
            for (index, param) in params.into_iter().enumerate() {
                match param {
                    OpeaqueType::Defined(param_type_kind) => {
                        let arg_exp = args.get(index).unwrap();
                        let arg_type_result = resolve_expr(*arg_exp.clone(), context);
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

pub fn resolve_binary(left: Expr, op: BinOpKind, right: Expr, context: &Context) -> TypeResult {
    match (left, right) {
        (Expr::Binary(l_left, l_op, l_right), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Binary(l_left, l_op, l_right), Expr::Unary(right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context);
            let r_resolved = resolve_type(right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Binary(l_left, l_op, l_right), Expr::Call(ids, args)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context);
            let r_resolved = resolve_call(ids, args, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Unary(left), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_type(left, context);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Unary(left), Expr::Unary(right)) => {
            let l_resolved = resolve_type(left, context);
            let r_resolved = resolve_type(right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Unary(left), Expr::Call(ids, args)) => {
            let l_resolved = resolve_type(left, context);
            let r_resolved = resolve_call(ids, args, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Call(ids, args), Expr::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_call(ids, args, context);
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Call(ids, args), Expr::Unary(right)) => {
            let l_resolved = resolve_call(ids, args, context);
            let r_resolved = resolve_type(right, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Expr::Call(left_ids, left_args), Expr::Call(right_ids, right_args)) => {
            let l_resolved = resolve_call(left_ids, left_args, context);
            let r_resolved = resolve_call(right_ids, right_args, context);
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        _ => unreachable!(),
    }
}

pub fn resolve_type(uni: Uni, context: &Context) -> TypeResult {
    match uni {
        Uni::Id(id) => match context.type_map.borrow_mut().try_get(&id) {
            Some(result @ TypeResult::Resolved(_)) => result.clone(),
            _ => TypeResult::IdOnly(id),
        },
        Uni::String(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
        Uni::Array(unis) => resolve_array(unis, context),
        Uni::Field(_) => unimplemented!(),
        Uni::HashMap(_) => unimplemented!(),
        Uni::Null => unimplemented!(),
    }
}

pub fn resolve_array(mut unis: Vec<Uni>, context: &Context) -> TypeResult {
    if unis.len() == 0 {
        TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Array(
            ArrayType::Unknown,
        )))
    } else {
        let array_type_result = resolve_type(unis.pop().unwrap(), context);
        for uni in unis {
            let elem_type_result = resolve_type(uni, context);

            match (&array_type_result, &elem_type_result) {
                (TypeResult::Resolved(_), TypeResult::Resolved(_)) => {
                    if elem_type_result != array_type_result {
                        return TypeResult::Err(create_conflict_array_elemenet_type_err(
                            &elem_type_result,
                            &array_type_result,
                        ));
                    }
                }
                (TypeResult::Resolved(_), TypeResult::IdOnly(id)) => {
                    let _ = context
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), array_type_result.clone())
                        .map_err(|err_str| return TypeResult::Err(err_str.to_string()));
                }
                (TypeResult::IdOnly(id), TypeResult::Resolved(_)) => {
                    let _ = context
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), elem_type_result.clone())
                        .map_err(|err_str| return TypeResult::Err(err_str.to_string()));
                }
                (TypeResult::Err(err_str), _) | (_, TypeResult::Err(err_str)) => {
                    return TypeResult::Err(err_str.to_string());
                }
                _ => unimplemented!(),
            }
        }
        array_type_result
    }
}

pub fn resolve_type_result_with_op(
    left: TypeResult,
    op: BinOpKind,
    right: TypeResult,
    context: &Context,
) -> TypeResult {
    match (&left, &right) {
        (TypeResult::Resolved(ref left), TypeResult::Resolved(ref right)) => {
            check_left_op_right(left, op, right, context)
        }
        (TypeResult::IdOnly(id), TypeResult::Resolved(right_type)) => {
            try_insert_and_resolve_op(&right, right_type, &id, op, context)
        }
        (TypeResult::Resolved(left_type), TypeResult::IdOnly(id)) => {
            try_insert_and_resolve_op(&left, left_type, &id, op, context)
        }
        (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => {
            let left_result = filter_type_result(&left, &left_id, context)
                .map_err(|err_str| return TypeResult::Err(err_str))
                .unwrap();

            let right_result = filter_type_result(&right, &right_id, context)
                .map_err(|err_str| return TypeResult::Err(err_str))
                .unwrap();

            match (left_result, right_result) {
                (TypeResult::Resolved(left_type), TypeResult::Resolved(_)) => {
                    TypeResult::Resolved(left_type.clone())
                }
                (TypeResult::Resolved(left_type), TypeResult::IdOnly(right_id)) => {
                    try_insert_and_resolve_op(&left_result, left_type, &right_id, op, context)
                }
                (TypeResult::IdOnly(left_id), TypeResult::Resolved(right_type)) => {
                    try_insert_and_resolve_op(&right_result, right_type, &left_id, op, context)
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
                            context.type_map.borrow_mut().insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            context.type_map.borrow_mut().insert(
                                right_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int))
                        }
                        BinOpKind::Lt | BinOpKind::Le | BinOpKind::Ge | BinOpKind::Gt => {
                            context.type_map.borrow_mut().insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            context.type_map.borrow_mut().insert(
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
    context: &Context,
) -> TypeResult {
    let _ = context
        .type_map
        .borrow_mut()
        .try_insert(id.clone(), original.clone())
        .map_err(|err_str| return TypeResult::Err(err_str));
    match resolve_op_one_side(&original_type, op, context) {
        Ok(_) => TypeResult::Resolved(original_type.clone()),
        Err(err_str) => TypeResult::Err(err_str),
    }
}

// return Resolved or Unknown only
pub fn filter_type_result<'a>(
    original: &'a TypeResult,
    id: &Id,
    context: &Context,
) -> Result<&'a TypeResult, String> {
    match context.type_map.borrow_mut().try_get(id) {
        Some(TypeResult::Resolved(_)) | Some(TypeResult::IdOnly(_)) => Ok(original),
        None => {
            context
                .type_map
                .borrow_mut()
                .insert(id.clone(), TypeResult::IdOnly(id.clone()));
            Ok(original)
        }
        Some(TypeResult::Unknown) => unimplemented!(),
        Some(TypeResult::Err(err_str)) => Err(err_str.to_string()),
        Some(TypeResult::Binary(_, _, _)) => unreachable!(),
    }
}

pub fn check_left_op_right(
    left: &TypeKind,
    op: BinOpKind,
    right: &TypeKind,
    context: &Context,
) -> TypeResult {
    if left == right {
        match resolve_op(&left, op, &right, context) {
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
    _context: &Context,
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
    _context: &Context,
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
            let mut context = Context::new();
            if let Ok((statements, _)) = ast().easy_parse($input) {
                assert_eq!(resolve_statement(statements, &mut context), $expected);
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

        let input = r#"let abc = 123
          def = "str"
        in (
          abc + def;
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
    fn let_nest() {
        let input = r#"let abc = 123 in (
          abc = 456;
          let def = abc + 789 in (
              abc + def + "nya-n";
          )
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
    fn call_infer() {
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
    fn call_infer_mismatch() {
        let input = r#"
            let abc = def() in (
                abc + 34;
                abc + "ghi"
            )
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            ))
        );

        let input = r#"
            let abc = fn (a) {
              a + 3;
            } in (
              abc("str");
            )
        "#;
        assert_infer!(
            input,
            Err(create_param_and_arg_type_is_mismatch_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
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

        let input = r#"
            fn(abc) {
                if (abc) {
                  return 123;
                }
                return 456;
            }
        "#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"
            fn(abc) {
                if (abc) {
                  return 123;
                }
                return true;
            }
        "#;
        assert_infer!(
            input,
            Err(create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            ))
        );
    }

    #[test]
    fn array_infer() {
        let input = r#"
            [1, "abc"];
        "#;
        assert_infer!(
            input,
            Err(create_conflict_array_elemenet_type_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
            ))
        );

        let input = r#"
            fn(a) {
                [a, 12];
                a + "abc";
            }
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            ))
        );
    }
}
