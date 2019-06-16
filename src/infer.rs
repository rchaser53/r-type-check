use std::env;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::scope::*;
use crate::statement::*;
use crate::types::*;

use crate::DEBUG_INFO;

#[derive(Clone, Debug)]
pub struct Context {
    pub scope: LocalScope,
}
impl Context {
    pub fn new() -> Self {
        Context {
            scope: LocalScope::new(None),
        }
    }
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
                    let right_type_result = resolve_expr(exp.clone(), &context)?;

                    match exp.node {
                        Node::Fn(function) => {
                            context
                                .scope
                                .function_map
                                .borrow_mut()
                                .insert(id.clone(), function.clone());
                        }
                        _ => {}
                    };

                    context
                        .scope
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), right_type_result)?;
                }
                let unboxed_body = body.into_iter().map(|statement| *statement).collect();
                resolve_statement(unboxed_body, context)?;
            }
            Statement::Expr(expr) => {
                resolve_expr(expr, context)?;
            }
            Statement::Assign(Assign(id, expr)) => {
                resolve_assign(id, expr, context)?;
            }
            Statement::Return(expr) => {
                let type_result = resolve_expr(expr, context)?;
                return_type_results.push(type_result.clone());
            }
            Statement::If(if_tuples) => {
                for (if_condition, boxed_body) in if_tuples {
                    let if_condition_type_result = resolve_expr(if_condition, context)?;
                    match if_condition_type_result {
                        TypeResult::Resolved(type_kind) => {
                            if type_kind != TypeKind::PrimitiveType(PrimitiveType::Boolean) {
                                return Err(create_if_condition_not_boolean_err(&type_kind));
                            }
                        }
                        TypeResult::IdOnly(id) => {
                            context.scope.type_map.borrow_mut().try_insert(
                                id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(
                                    PrimitiveType::Boolean,
                                )),
                            )?;
                        }
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

pub fn resolve_assign(id: Id, exp: Expr, context: &Context) -> Result<TypeResult, String> {
    let right_type_result = resolve_expr(exp.clone(), context)?;
    if let Some(left_type) = context.scope.type_map.borrow_mut().try_get(&id) {
        match left_type {
            TypeResult::Resolved(left_type) => {
                if let Some(err_str) = validate_assign_type(left_type, &right_type_result) {
                    return Err(err_str);
                }
            }
            _ => {}
        };
    } else {
        return Err(create_not_initialized_err(&id));
    };
    context
        .scope
        .type_map
        .borrow_mut()
        .try_insert(id, right_type_result)
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

pub fn resolve_expr(exp: Expr, context: &Context) -> Result<TypeResult, String> {
    match exp.node {
        Node::Unary(uni) => resolve_uni(uni, context),
        Node::Binary(left, op, right) => resolve_binary(*left, op, *right, context),
        Node::Fn(Function(args, body)) => {
            let fn_context = Context::new();
            for arg in args.clone() {
                fn_context
                    .scope
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
                                fn_context.scope.type_map.borrow_mut().try_get(&id)
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
                    Ok(TypeResult::Resolved(TypeKind::Function(
                        exp.id.clone(),
                        fn_arg_types,
                        return_type,
                    )))
                }
                Err(err_str) => Err(err_str),
            }
        }
        Node::Call(field, boxed_args) => resolve_call(field, boxed_args, context),
    }
}

pub fn resolve_call(
    field: Field,
    args: Vec<Box<Expr>>,
    context: &Context,
) -> Result<TypeResult, String> {
    // TBD: need to implement correctly
    // especially for field
    // ex. xx.yy();
    let id = field.id.0.clone();
    let type_result = resolve_field(field, context)?;

    let arg_len = args.len();
    let mut arg_type_vec = Vec::with_capacity(arg_len);

    let (should_insert, ret_result) = match type_result {
        TypeResult::IdOnly(_) => {
            for index in 0..arg_len {
                arg_type_vec[index] = OpeaqueType::Unknown
            }
            (
                true,
                TypeResult::Resolved(TypeKind::Function(
                    id.clone(),
                    arg_type_vec.clone(),
                    OpeaqueType::Unknown,
                )),
            )
        }
        type_result @ _ => (false, type_result.clone()),
    };

    if should_insert {
        context.scope.type_map.borrow_mut().insert(
            id.clone(),
            TypeResult::Resolved(TypeKind::Function(
                id.clone(),
                arg_type_vec,
                OpeaqueType::Unknown,
            )),
        );
    }

    let fn_context = Context::new();
    let result = match ret_result {
        TypeResult::Resolved(TypeKind::Function(_, params, return_opeaque)) => {
            for (index, param) in params.into_iter().enumerate() {
                match param {
                    OpeaqueType::Defined(param_type_kind) => {
                        let arg_exp = args.get(index).unwrap();
                        let arg_type_result = resolve_expr(*arg_exp.clone(), &fn_context)?;
                        let param_type_result = TypeResult::Resolved(*param_type_kind.clone());
                        if arg_type_result != param_type_result {
                            return Err(create_param_and_arg_type_is_mismatch_err(
                                &arg_type_result,
                                &param_type_result,
                            ));
                        }
                    }
                    OpeaqueType::IdOnly(arg_id) => {
                        let arg_exp = args.get(index).unwrap();
                        let arg_type_result = resolve_expr(*arg_exp.clone(), &fn_context)?;
                        // save type information for paramerter in function
                        // and retry type check somewhere
                        fn_context
                            .scope
                            .type_map
                            .borrow_mut()
                            .insert(arg_id.clone(), arg_type_result);
                    }
                    _ => {
                        // TBD: need to check correctly
                    }
                }
            }

            match return_opeaque {
                OpeaqueType::Defined(boxed_type_kind) => {
                    Ok(TypeResult::Resolved(*boxed_type_kind.clone()))
                }
                OpeaqueType::IdOnly(_) | OpeaqueType::Unknown => Ok(TypeResult::Unknown),
            }
        }
        TypeResult::Resolved(type_kind @ _) => Err(create_cannot_call_err(&id, &type_kind)),
        TypeResult::IdOnly(_) | TypeResult::Unknown => Ok(TypeResult::Unknown),
        _ => unreachable!(),
    };

    let mut fn_map = context.scope.function_map.borrow_mut();
    let mut bodys = if let Some(Function(_, bodys)) = fn_map.get_mut(&id) {
        bodys
            .clone()
            .into_iter()
            .map(|boxed_statement| *boxed_statement)
            .collect()
    } else {
        return result;
    };

    DEBUG_INFO!("resolve_call", &fn_context);
    let fn_return_type_result = resolve_statement(bodys, &fn_context)?;
    let result = result?;

    match (fn_return_type_result, result.clone()) {
        (TypeResult::Resolved(left_type_kind), TypeResult::Resolved(right_type_kind)) => {
            if left_type_kind != right_type_kind {
                return Err(create_conflict_type_return_err(
                    &TypeResult::Resolved(left_type_kind),
                    &TypeResult::Resolved(right_type_kind),
                ));
            }
        }
        _ => { /* TBD: need implements correctly */ }
    }
    Ok(result)
}

pub fn resolve_binary(
    left: Expr,
    op: BinOpKind,
    right: Expr,
    context: &Context,
) -> Result<TypeResult, String> {
    let result = match (left.node, right.node) {
        (Node::Binary(l_left, l_op, l_right), Node::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context)?;
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Binary(l_left, l_op, l_right), Node::Unary(right)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context)?;
            let r_resolved = resolve_uni(right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Binary(l_left, l_op, l_right), Node::Call(r_field, args)) => {
            let l_resolved = resolve_binary(*l_left, l_op, *l_right, context)?;
            let r_resolved = resolve_call(r_field, args, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Unary(left), Node::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_uni(left, context)?;
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Unary(left), Node::Unary(right)) => {
            let l_resolved = resolve_uni(left, context)?;
            let r_resolved = resolve_uni(right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Unary(left), Node::Call(r_field, args)) => {
            let l_resolved = resolve_uni(left, context)?;
            let r_resolved = resolve_call(r_field, args, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, args), Node::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_call(l_field, args, context)?;
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, args), Node::Unary(right)) => {
            let l_resolved = resolve_call(l_field, args, context)?;
            let r_resolved = resolve_uni(right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, left_args), Node::Call(r_field, right_args)) => {
            let l_resolved = resolve_call(l_field, left_args, context)?;
            let r_resolved = resolve_call(r_field, right_args, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        _ => unreachable!(),
    };
    result
}

pub fn resolve_uni(uni: Uni, context: &Context) -> Result<TypeResult, String> {
    let result = match uni {
        Uni::Id(id) => match context.scope.type_map.borrow_mut().try_get(&id) {
            Some(result @ TypeResult::Resolved(_)) => result.clone(),
            _ => TypeResult::IdOnly(id),
        },
        Uni::String(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
        Uni::Array(unis) => resolve_array(unis, context)?,
        Uni::HashMap(hash) => resolve_hash(hash, context)?,
        Uni::Field(field) => resolve_field(field, context)?,
        Uni::Null => unimplemented!(),
    };
    Ok(result)
}

/// xxx.yyy comes now. xxx is possibility for every type
pub fn resolve_field(field: Field, context: &Context) -> Result<TypeResult, String> {
    DEBUG_INFO!("resolve_field", &context);

    let current_id = field.id.0.clone();
    if let Some(child) = field.child {
        let child_id = child.id.clone();
        // try to get object scope id
        if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&current_id) {
            match type_result {
                TypeResult::Resolved(TypeKind::Object(object_id)) => {
                    return resolve_object(object_id, &child_id.0, context);
                }
                TypeResult::Resolved(type_kind @ _) => {
                    // check property for primitive type
                    return resolve_unique_field(&current_id, &child_id.0, &type_kind);
                }
                _ => {}
            };
            Ok(type_result.clone())
        } else {
            unimplemented!();
        }
    } else {
        // case xxx()
        if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&current_id) {
            Ok(type_result.clone())
        } else {
            // case xxx.yyy()
            // Err(create_not_initialized_err(&current_id))
            Ok(TypeResult::Unknown)
        }
    }
}

/// try to get object scope
pub fn resolve_object(
    object_scope_id: &ObjectId,
    id: &Id,
    context: &Context,
) -> Result<TypeResult, String> {
    if let Some(boxed_scope) = context
        .scope
        .scope_map
        .borrow_mut()
        .get_mut(&IdType::Object(object_scope_id.clone()))
    {
        if let Scope::Object(object_map) = *boxed_scope.clone() {
            // try to get field type_result
            if let Some(type_result) = object_map.type_map.borrow_mut().try_get(&id) {
                return Ok(type_result.clone());
            } else {
                unimplemented!()
            }
        } else {
            unreachable!()
        }
    } else {
        // Maybe...
        unreachable!()
    }
}

pub fn resolve_unique_field(
    parent_id: &Id,
    id: &Id,
    type_kind: &TypeKind,
) -> Result<TypeResult, String> {
    return match type_kind {
        TypeKind::PrimitiveType(PrimitiveType::Int) => {
            if &Id(String::from("length")) == id {
                return Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                    PrimitiveType::Int,
                )));
            } else {
                Err(create_undefined_field_err(parent_id, id))
            }
        }
        result @ _ => Ok(TypeResult::Resolved(result.clone())),
    };
}

pub fn resolve_array(mut unis: Vec<Uni>, context: &Context) -> Result<TypeResult, String> {
    if unis.len() == 0 {
        Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Array(ArrayType::Unknown),
        )))
    } else {
        let array_type_result = resolve_uni(unis.pop().unwrap(), context)?;
        for uni in unis {
            let elem_type_result = resolve_uni(uni, context)?;

            match (&array_type_result, &elem_type_result) {
                (TypeResult::Resolved(_), TypeResult::Resolved(_)) => {
                    if elem_type_result != array_type_result {
                        return Err(create_conflict_array_elemenet_type_err(
                            &elem_type_result,
                            &array_type_result,
                        ));
                    }
                }
                (TypeResult::Resolved(_), TypeResult::IdOnly(id)) => {
                    context
                        .scope
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), array_type_result.clone())?;
                }
                (TypeResult::IdOnly(id), TypeResult::Resolved(_)) => {
                    context
                        .scope
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), elem_type_result.clone())?;
                }
                _ => unimplemented!(),
            }
        }
        Ok(array_type_result)
    }
}

pub fn resolve_hash(hash: Hash, context: &Context) -> Result<TypeResult, String> {
    let parent_id = context.scope.id.0.clone();
    let hash_scope = ObjectScope::new(Some(IdType::Local(ScopeId(parent_id))));
    let hash_scope_id = hash_scope.id.clone();
    let hash_map = hash.1;
    for (key, boxed_exp) in hash_map.into_iter() {
        if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&key) {
            hash_scope
                .type_map
                .borrow_mut()
                .insert(key.clone(), type_result.clone());
        } else {
            let type_result = resolve_expr(*boxed_exp, context)?;
            hash_scope
                .type_map
                .borrow_mut()
                .insert(key.clone(), type_result);
        }
    }
    context.scope.scope_map.borrow_mut().insert(
        IdType::Object(hash_scope_id.clone()),
        Box::new(Scope::Object(hash_scope)),
    );
    Ok(TypeResult::Resolved(TypeKind::Object(hash_scope_id)))
}

pub fn resolve_type_result_with_op(
    left: TypeResult,
    op: BinOpKind,
    right: TypeResult,
    context: &Context,
) -> Result<TypeResult, String> {
    match (&left, &right) {
        (TypeResult::Resolved(ref left), TypeResult::Resolved(ref right)) => {
            check_left_op_right(left, op, right, context)
        }
        (TypeResult::Resolved(left_type), TypeResult::IdOnly(id)) => {
            try_insert_and_resolve_op(&left, left_type, &id, op, context)
        }
        (TypeResult::Resolved(_), TypeResult::Unknown) => Ok(left.clone()),
        (TypeResult::IdOnly(id), TypeResult::Resolved(right_type)) => {
            try_insert_and_resolve_op(&right, right_type, &id, op, context)
        }
        (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => {
            let left_result = filter_type_result(&left, &left_id, context)?;
            let right_result = filter_type_result(&right, &right_id, context)?;

            match (left_result, right_result) {
                (TypeResult::Resolved(left_type), TypeResult::Resolved(_)) => {
                    Ok(TypeResult::Resolved(left_type.clone()))
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
                            context.scope.type_map.borrow_mut().insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            context.scope.type_map.borrow_mut().insert(
                                right_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                                PrimitiveType::Int,
                            )))
                        }
                        BinOpKind::Lt | BinOpKind::Le | BinOpKind::Ge | BinOpKind::Gt => {
                            context.scope.type_map.borrow_mut().insert(
                                left_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            context.scope.type_map.borrow_mut().insert(
                                right_id.clone(),
                                TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                            );
                            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                                PrimitiveType::Boolean,
                            )))
                        }
                        _ => Ok(TypeResult::IdOnly(left_id.clone())),
                    }
                }
                _ => unreachable!(),
            }
        }
        (TypeResult::Unknown, TypeResult::Resolved(_)) => Ok(right.clone()),
        (TypeResult::Unknown, _) => Ok(TypeResult::Unknown),
        (_, TypeResult::Unknown) => Ok(TypeResult::Unknown),
        _ => unimplemented!(),
    }
}

pub fn try_insert_and_resolve_op(
    original: &TypeResult,
    original_type: &TypeKind,
    id: &Id,
    op: BinOpKind,
    context: &Context,
) -> Result<TypeResult, String> {
    context
        .scope
        .type_map
        .borrow_mut()
        .try_insert(id.clone(), original.clone())?;
    match resolve_op_one_side(&original_type, op, context) {
        Ok(_) => Ok(TypeResult::Resolved(original_type.clone())),
        Err(err_str) => Err(err_str),
    }
}

// return Resolved or Unknown only
pub fn filter_type_result<'a>(
    original: &'a TypeResult,
    id: &Id,
    context: &Context,
) -> Result<&'a TypeResult, String> {
    if let Some(result) = context.scope.type_map.borrow_mut().try_get(id) {
        return match result {
            TypeResult::Resolved(_) | TypeResult::IdOnly(_) => Ok(original),
            TypeResult::Unknown => unimplemented!(),
            TypeResult::Binary(_, _, _) => unreachable!(),
        };
    }
    context
        .scope
        .type_map
        .borrow_mut()
        .insert(id.clone(), TypeResult::IdOnly(id.clone()));
    Ok(original)
}

pub fn check_left_op_right(
    left: &TypeKind,
    op: BinOpKind,
    right: &TypeKind,
    context: &Context,
) -> Result<TypeResult, String> {
    if left == right {
        match resolve_op(&left, op, &right, context) {
            Ok(result) => Ok(result),
            Err(err_str) => Err(err_str),
        }
    } else {
        Err(create_type_mismatch_err(&left, &right))
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
    use combine::Parser;

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
    fn let_definition() {
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

        let input = r#"let
        abc = 123
        abc = "abc" in ()"#;
        // TODO: need to improve error message
        assert_infer!(
            input,
            Err(create_assign_conflict_type_err(
                &Id(String::from("abc")),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            ))
        );
    }

    #[test]
    fn let_body() {
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
                &TypeKind::Function(Id(String::from("whatever")), vec![], OpeaqueType::Unknown),
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
    fn polymofism_test() {
        let input = r#"
            let abc = fn(a){ return a; } in (
                abc(1);
                abc("a");
            )
        "#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"
            let abc = fn (def, ghi){ return def + ghi; } in (
                abc(2, 1) + 33;
                abc("a", "b") + "cde";
                abc("a", true);
            )
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::String),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
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

    #[test]
    fn hash_map_infer() {
        let input = r#"
            { abc: "def" };
        "#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"
            let abc = { def: 123 } in (
                if (true) {
                  return abc.def;
                }
                return abc.def + 456;
            )
        "#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );
    }

    #[test]
    fn hash_map_call_infer() {
        let input = r#"
            let abc = {
              def: fn() {
                return 3;
              }
            } in (
                abc.def() + "abc";
            )
        "#;
        assert_infer!(
            input,
            Err(create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            ))
        );
    }

    #[test]
    fn field_for_primitive_type() {
        let input = r#"
            let abc = 123 in (
                111 + abc.length;
            )
        "#;
        assert_infer!(
            input,
            Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Void
            )))
        );

        let input = r#"
            let abc = 123 in (
                abc.nothing;
            )
        "#;
        assert_infer!(
            input,
            Err(create_undefined_field_err(
                &Id(String::from("abc")),
                &Id(String::from("nothing")),
            ))
        );
    }
}
