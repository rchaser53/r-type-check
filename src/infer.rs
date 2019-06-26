use std::cell::RefCell;
use std::env;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::scope::*;
use crate::statement::*;
use crate::types::*;

use crate::DEBUG_INFO;

pub mod array;
pub mod boolean;
pub mod int;
pub mod string;
use array::*;
use boolean::*;
use int::*;
use string::*;

#[derive(Clone, Debug)]
pub struct Context {
    pub scope: LocalScope,
    pub current_left_id: RefCell<Option<Id>>,
}
impl Context {
    pub fn new() -> Self {
        Context {
            current_left_id: RefCell::new(None),
            scope: LocalScope::new(None),
        }
    }
}

type Result<T> = std::result::Result<T, TypeError>;

/// return function return TypeResult
pub fn resolve_statement(statements: Vec<Statement>, context: &Context) -> Result<TypeResult> {
    let mut return_type_results = vec![];
    for statement in statements {
        match statement.node {
            StmtKind::Let(lets, body) => {
                for Assign(id, exp) in lets {
                    context.current_left_id.replace(Some(id.clone()));
                    let right_type_result = resolve_expr(exp.clone(), &context)?;

                    if let Node::Fn(function) = exp.node {
                        context
                            .scope
                            .function_map
                            .borrow_mut()
                            .insert(id.clone(), function.clone());
                    };

                    context
                        .scope
                        .type_map
                        .borrow_mut()
                        .try_insert(id.clone(), right_type_result)?;
                }
                context.current_left_id.replace(None);
                resolve_statement(body, context)?;
            }
            StmtKind::Expr(expr) => {
                resolve_expr(expr, context)?;
            }
            StmtKind::Assign(Assign(id, expr)) => {
                context.current_left_id.replace(Some(id.clone()));
                resolve_assign(id, expr, context)?;
                context.current_left_id.replace(None);
            }
            StmtKind::Return(expr) => {
                let position = expr.position.lo;
                let type_result = resolve_expr(expr, context)?;
                return_type_results.push((type_result.clone(), position));
            }
            StmtKind::If(if_tuples) => {
                for (if_condition, boxed_body) in if_tuples {
                    let position = if_condition.position.lo;
                    let if_condition_type_result = resolve_expr(if_condition, context)?;
                    match if_condition_type_result {
                        TypeResult::Resolved(type_kind) => {
                            if type_kind != TypeKind::PrimitiveType(PrimitiveType::Boolean) {
                                let mut err = create_if_condition_not_boolean_err(&type_kind);
                                err.set_pos(position);
                                return Err(err);
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
                    let unboxed_body = boxed_body;
                    // get return type in if statement
                    let if_return_type_result = resolve_statement(unboxed_body, context)?;
                    return_type_results.push((if_return_type_result, position));
                }
            }
        };
    }

    if return_type_results.is_empty() {
        Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Void,
        )))
    } else {
        let (first_type_result, _) = return_type_results.pop().unwrap();
        for (return_type_result, position) in return_type_results {
            if first_type_result != return_type_result {
                let mut err =
                    create_conflict_type_return_err(&first_type_result, &return_type_result);
                err.set_pos(position);
                return Err(err);
            }
        }
        Ok(first_type_result)
    }
}

pub fn resolve_assign(id: Id, exp: Expr, context: &Context) -> Result<TypeResult> {
    let position = exp.position.lo;
    let right_type_result = resolve_expr(exp.clone(), context)?;
    if let Some(left_type) = context.scope.type_map.borrow_mut().try_get(&id) {
        if let TypeResult::Resolved(left_type) = left_type {
            if let Some(mut err) = validate_assign_type(left_type, &right_type_result) {
                err.set_pos(position);
                return Err(err);
            }
        };
    } else {
        let mut err = create_not_initialized_err(&id);
        err.set_pos(position);
        return Err(err);
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
) -> Option<TypeError> {
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

pub fn resolve_expr(exp: Expr, context: &Context) -> Result<TypeResult> {
    let exp_id = exp.id.clone();
    let position = exp.position.lo;
    let result = match exp.node {
        Node::Unary(uni) => resolve_uni(uni, context),
        Node::Binary(left, op, right) => resolve_binary(*left, op, *right, context),
        Node::Fn(Function(args, body)) => resolve_fn(exp_id, args, body, context),
        Node::Call(field, boxed_args) => resolve_call(field, boxed_args, context),
    };

    match result {
        Ok(result) => Ok(result),
        Err(mut err) => {
            err.set_pos(position);
            Err(err)
        }
    }
}

pub fn resolve_fn(
    id: Id,
    args: Vec<Id>,
    body: Vec<Statement>,
    _context: &Context,
) -> Result<TypeResult> {
    let fn_context = Context::new();
    for arg in args.clone() {
        fn_context
            .scope
            .type_map
            .borrow_mut()
            .insert(arg.clone(), TypeResult::IdOnly(arg));
    }
    let result = resolve_statement(body, &fn_context)?;

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
        TypeResult::Resolved(return_type) => OpeaqueType::Defined(Box::new(return_type)),
        TypeResult::IdOnly(id) => OpeaqueType::IdOnly(id),
        _ => unreachable!(),
    };
    // TBD: need to think more
    Ok(TypeResult::Resolved(TypeKind::Function(
        id,
        fn_arg_types,
        return_type,
    )))
}

pub fn resolve_call(field: Field, args: Vec<Expr>, context: &Context) -> Result<TypeResult> {
    // TBD: need to implement correctly
    // especially for field
    // ex. xx.yy();
    let id = get_id(field.id.clone(), field.child.clone()).0;
    let type_result = resolve_field(field, vec![], context)?;

    let arg_len = args.len();
    let mut arg_type_vec = Vec::with_capacity(arg_len);

    let ret_result = match type_result {
        TypeResult::IdOnly(_) => {
            for item in arg_type_vec.iter_mut().take(arg_len) {
                *item = OpeaqueType::Unknown
            }

            let type_result = TypeResult::Resolved(TypeKind::Function(
                id.clone(),
                arg_type_vec,
                OpeaqueType::Unknown,
            ));

            context
                .scope
                .type_map
                .borrow_mut()
                .insert(id.clone(), type_result.clone());

            type_result
        }
        type_result => type_result,
    };

    let fn_context = Context::new();
    let result = match ret_result {
        TypeResult::Resolved(TypeKind::Function(_, params, return_opeaque)) => {
            for (index, param) in params.into_iter().enumerate() {
                match param {
                    OpeaqueType::Defined(param_type_kind) => {
                        let arg_exp = args.get(index).unwrap();
                        let arg_type_result = resolve_expr(arg_exp.clone(), &fn_context)?;
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
                        let arg_type_result = resolve_expr(arg_exp.clone(), &fn_context)?;
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
        TypeResult::Resolved(type_kind) => Err(create_cannot_call_err(&id, &type_kind)),
        TypeResult::IdOnly(_) | TypeResult::Unknown => Ok(TypeResult::Unknown),
        _ => unreachable!(),
    }?;

    let mut fn_map = context.scope.function_map.borrow_mut();
    let bodys = if let Some(Function(_, bodys)) = fn_map.get_mut(&id) {
        bodys.to_vec()
    } else {
        return Ok(result);
    };

    DEBUG_INFO!("resolve_call", &fn_context);
    let fn_return_type_result = resolve_statement(bodys, &fn_context)?;

    match (&fn_return_type_result, &result) {
        (TypeResult::Resolved(left_type_kind), TypeResult::Resolved(right_type_kind)) => {
            if left_type_kind != right_type_kind {
                return Err(create_conflict_type_return_err(
                    &TypeResult::Resolved(left_type_kind.clone()),
                    &TypeResult::Resolved(right_type_kind.clone()),
                ));
            }
            Ok(fn_return_type_result)
        }
        _ => Ok(fn_return_type_result),
    }
}

pub fn get_id(id: ObjectId, field: Option<Box<Field>>) -> ObjectId {
    if let Some(field) = field {
        get_id(field.id.clone(), field.child)
    } else {
        id
    }
}

pub fn resolve_binary(
    left: Expr,
    op: BinOpKind,
    right: Expr,
    context: &Context,
) -> Result<TypeResult> {
    match (left.node, right.node) {
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
    }
}

pub fn resolve_uni(uni: Uni, context: &Context) -> Result<TypeResult> {
    let result = match uni {
        Uni::Id(id) => match context.scope.type_map.borrow_mut().try_get(&id) {
            Some(result @ TypeResult::Resolved(_)) => result.clone(),
            _ => TypeResult::IdOnly(id),
        },
        Uni::String(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
        Uni::Number(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
        Uni::Boolean(_) => TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
        Uni::Array(unis) => resolve_array(unis, context)?,
        Uni::HashMap(hash) => resolve_hash(hash, None, context)?,
        Uni::Field(field) => resolve_field(field, vec![], context)?,
        Uni::Null => unimplemented!(),
    };
    Ok(result)
}

/// xxx.yyy comes now. xxx is possibility for every type
pub fn resolve_field(
    field: Field,
    mut field_object_ids: Vec<ObjectId>,
    context: &Context,
) -> Result<TypeResult> {
    DEBUG_INFO!("resolve_field", &context);

    let current_id = field.id.clone();
    if let Some(child) = field.child {
        // try to get object scope id
        let type_result =
            if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&current_id.0) {
                Some(type_result.clone())
            } else {
                None
            };

        let type_result = if type_result.is_none() {
            field_object_ids.push(current_id.clone());
            return resolve_field(*child, field_object_ids, context);
        } else {
            type_result.unwrap()
        };

        resolve_field_object(*child, current_id.0, type_result, context)
    } else {
        if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&current_id.0) {
            return Ok(type_result.clone());
        }

        // TBD: need to implement multi nest
        if field_object_ids.is_empty() {
            return Ok(TypeResult::Unknown);
        }

        let (first_id, field_object_ids) = field_object_ids.split_first().unwrap();
        let mut result_scope = if let Some(scope) = context
            .scope
            .scope_map
            .borrow_mut()
            .get(&IdType::Object(first_id.clone()))
        {
            match *scope.clone() {
                Scope::Local(_) => unreachable!(),
                Scope::Object(object_scope) => object_scope,
            }
        } else {
            return Ok(TypeResult::Unknown);
        };

        for resolve_id in field_object_ids {
            let temp_scope =
                if let Some(scope) = result_scope.scope_map.borrow_mut().get(resolve_id) {
                    scope.clone()
                } else {
                    // scope doesn't find
                    return Ok(TypeResult::Unknown);
                };
            result_scope = *temp_scope;
        }

        match result_scope
            .clone()
            .type_map
            .borrow_mut()
            .try_get(&current_id.0)
            .cloned()
        {
            Some(result) => Ok(result),
            _ => Ok(TypeResult::IdOnly(current_id.0)),
        }
    }
}

pub fn resolve_field_object(
    field: Field,
    current_id: Id,
    type_result: TypeResult,
    context: &Context,
) -> Result<TypeResult> {
    match type_result {
        TypeResult::Resolved(TypeKind::Scope(id_type)) => match id_type {
            IdType::Object(object_id) => {
                let type_result = resolve_object(&object_id, &field.id.0, context);
                let current_id = field.id.0.clone();
                if let Some(child) = field.child {
                    resolve_field_object(*child, current_id, type_result?, context)
                } else {
                    type_result
                }
            }
            IdType::Local(_) => unimplemented!(),
        },
        TypeResult::Resolved(type_kind) => {
            // check property for primitive type
            resolve_unique_field(&current_id, &field.id.0, &type_kind, context)
        }
        type_result => Ok(type_result),
    }
}

/// try to get object scope
pub fn resolve_object(
    object_scope_id: &ObjectId,
    id: &Id,
    context: &Context,
) -> Result<TypeResult> {
    if let Some(boxed_scope) = context
        .scope
        .scope_map
        .borrow_mut()
        .get_mut(&IdType::Object(object_scope_id.clone()))
    {
        if let Scope::Object(object_map) = *boxed_scope.clone() {
            // try to get field type_result
            if let Some(type_result) = object_map.type_map.borrow_mut().try_get(&id) {
                Ok(type_result.clone())
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
    context: &Context,
) -> Result<TypeResult> {
    match type_kind {
        TypeKind::PrimitiveType(PrimitiveType::Int) => resolve_int_method(parent_id, id, context),
        TypeKind::PrimitiveType(PrimitiveType::String) => {
            resolve_string_method(parent_id, id, context)
        }
        TypeKind::PrimitiveType(PrimitiveType::Boolean) => {
            resolve_boolean_method(parent_id, id, context)
        }
        TypeKind::PrimitiveType(PrimitiveType::Array(_)) => {
            resolve_array_method(parent_id, id, context)
        }
        result => Ok(TypeResult::Resolved(result.clone())),
    }
}

pub fn resolve_array(mut unis: Vec<Uni>, context: &Context) -> Result<TypeResult> {
    if unis.is_empty() {
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
        match array_type_result {
            TypeResult::Resolved(TypeKind::PrimitiveType(primitive_type)) => {
                Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                    PrimitiveType::Array(ArrayType::Defined(Box::new(primitive_type))),
                )))
            }
            _ => unimplemented!(),
        }
    }
}

pub fn resolve_hash(
    hash: Hash,
    parent_id: Option<IdType>,
    context: &Context,
) -> Result<TypeResult> {
    let hash_scope = ObjectScope::new(parent_id, None);
    let hash_scope_id = if let Some(left_id) = context.current_left_id.borrow_mut().clone() {
        ObjectId(left_id.clone())
    } else {
        // this is a case StmtKind::Expr(Expr::Unary(Uni::Hash))
        // so it's not used. like below
        // { abc: 123 };
        return Ok(TypeResult::Unknown);
    };

    for (key, boxed_exp) in hash.0.into_iter() {
        context.current_left_id.replace(Some(key.clone()));
        let type_result = resolve_expr(*boxed_exp.clone(), context)?;

        let type_result = match type_result {
            // the case for { abc: { def: xxx } }
            TypeResult::Resolved(TypeKind::Scope(id_type)) => match id_type {
                IdType::Local(_local_id) => unimplemented!(),
                IdType::Object(object_id) => {
                    context.current_left_id.replace(Some(object_id.0.clone()));
                    match (*boxed_exp).clone().node {
                        Node::Unary(Uni::HashMap(hash)) => resolve_hash(
                            hash,
                            Some(IdType::Object(hash_scope_id.clone())),
                            context,
                        )?,
                        _ => resolve_expr(*boxed_exp, context)?,
                    }
                }
            },
            type_result => type_result,
        };

        hash_scope
            .type_map
            .borrow_mut()
            .insert(key.clone(), type_result);
        context.current_left_id.replace(None);
    }

    context.scope.scope_map.borrow_mut().insert(
        IdType::Object(hash_scope_id.clone()),
        Box::new(Scope::Object(hash_scope)),
    );
    Ok(TypeResult::Resolved(TypeKind::Scope(IdType::Object(
        hash_scope_id,
    ))))
}

pub fn resolve_type_result_with_op(
    left: TypeResult,
    op: BinOpKind,
    right: TypeResult,
    context: &Context,
) -> Result<TypeResult> {
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
                (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => match op {
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
                },
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
) -> Result<TypeResult> {
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
) -> Result<&'a TypeResult> {
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
) -> Result<TypeResult> {
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
) -> Result<TypeResult> {
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

pub fn resolve_op_one_side(oneside: &TypeKind, op: BinOpKind, _context: &Context) -> Result<()> {
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

#[cfg(test)]
mod test {
    use crate::ast::*;
    use crate::infer::*;

    use combine::stream::state::State;
    use combine::Parser;

    macro_rules! assert_infer {
        ($input: expr, $expected: expr) => {
            let mut context = Context::new();
            match ast().easy_parse(State::new($input)) {
                Ok((statements, _)) => match resolve_statement(statements, &mut context) {
                    Ok(result) => assert_eq!(result, $expected),
                    Err(_) => unreachable!(),
                },
                Err(err) => panic!("{:?}", err),
            }
        };
    }

    macro_rules! assert_infer_err {
        ($input: expr, $expected: expr) => {
            let mut context = Context::new();
            match ast().easy_parse(State::new($input)) {
                Ok((statements, _)) => match resolve_statement(statements, &mut context) {
                    Ok(_) => unreachable!("should not be ok"),
                    Err(err) => assert_eq!(err.message, $expected.message),
                },
                Err(err) => panic!("{:?}", err),
            }
        };
    }

    #[test]
    fn let_definition() {
        let input = r#"let abc = 123 + "abc" in (
        )"#;
        // TODO: need to improve error message
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );

        let input = r#"let
        abc = 123
        abc = "abc" in ()"#;
        // TODO: need to improve error message
        assert_infer_err!(
            input,
            create_assign_conflict_type_err(
                &Id(String::from("abc")),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn let_body() {
        let input = r#"let abc = 123 in (
          abc + 456;
        )"#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"let abc = def in (
          abc + 456;
          abc + "def";
        )"#;
        // TODO: need to improve error message
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );

        let input = r#"let abc = true in (
          abc = (123 > 456) + 44;
        )"#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int)
            )
        );

        let input = r#"let abc = def in (
          (abc + 234) != (abc == false);
        )"#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean)
            )
        );
    }

    #[test]
    fn let_assign_uninitialized() {
        let input = r#"let abc = 123 in (
          def = 456;
        )"#;
        assert_infer_err!(input, create_not_initialized_err(&Id(String::from("def"))));
    }

    #[test]
    fn let_assign_type_mismatch() {
        let input = r#"let abc = 123 in (
          abc = 456;
          abc = "err";
        )"#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );

        let input = r#"let abc = 123
          def = "str"
        in (
          abc + def;
        )"#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
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
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn binary_type_mismatch() {
        let input = r#"123 + "abc""#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn binary_op_mismatch() {
        let input = r#""def" - "abc""#;
        assert_infer_err!(
            input,
            create_cannot_use_op_err(
                &TypeKind::PrimitiveType(PrimitiveType::String),
                BinOpKind::Sub,
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
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
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"fn (aaa, bbb) {
            aaa + 123;
            bbb + 456;
            aaa + bbb + "abc";
        }"#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn return_infer_incorrect() {
        let input = r#"
            return 123;
            return "abc";
        "#;
        assert_infer_err!(
            input,
            create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            )
        );

        let input = r#"
            fn() {
              return 123;
              return "abc";
            }
        "#;
        assert_infer_err!(
            input,
            create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            )
        );

        let input = r#"
            fn(abc, def) {
              abc * def;
              return abc == true;
            }
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
            )
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
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
        );

        let input = r#"
            let test = fn(abc) {
              return abc == true;
            } in (
              test(123);
            );
        "#;
        assert_infer_err!(
            input,
            create_param_and_arg_type_is_mismatch_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
            )
        );

        let input = r#"
            fn(abc) {
              abc = 12;
              abc();
            }
        "#;
        assert_infer_err!(
            input,
            create_cannot_call_err(
                &Id(String::from("abc")),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
        );

        let input = r#"
            fn(abc) {
              abc();
              abc = 12;
            }
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::Function(Id(String::from("whatever")), vec![], OpeaqueType::Unknown),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
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
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );

        let input = r#"
            let abc = fn (a) {
              a + 3;
            } in (
              abc("str");
            )
        "#;
        assert_infer_err!(
            input,
            create_param_and_arg_type_is_mismatch_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            )
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
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            let abc = fn (def, ghi){ return def + ghi; } in (
                abc(2, 1) + 33;
                abc("a", "b") + "cde";
                abc("a", true);
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::String),
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
            )
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
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Boolean),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
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
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            fn(abc) {
                if (abc) {
                  return 123;
                }
                return true;
            }
        "#;
        assert_infer_err!(
            input,
            create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
            )
        );
    }

    #[test]
    fn array_infer() {
        let input = r#"
            [1, "abc"];
        "#;
        assert_infer_err!(
            input,
            create_conflict_array_elemenet_type_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
            )
        );

        let input = r#"
            fn(a) {
                [a, 12];
                a + "abc";
            }
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );

        let input = r#"
            let abc = [1,2,3] in (
                abc.pop() + "abc";
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );
    }

    #[test]
    fn hash_map_infer() {
        let input = r#"
            { abc: "def" };
        "#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
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
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );
    }

    #[test]
    fn hash_map_nest_infer() {
        let input = r#"
            let abc = { def: {
              ghi: 123
            } } in (
                if (true) {
                  return 456;
                }
                return abc.def.ghi;
            )
        "#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            let abc = { def: {
              ghi: {
                jkl: 123
              },
              ghi2: 222
            } } in (
                if (true) {
                  return abc.def.ghi2 + 222;
                }
                return 111 * abc.def.ghi.jkl;
            )
        "#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            let abc = { def: {
              ghi: {
                jkl: 123
              },
              ghi2: 222
            } } in (
                if (true) {
                  return "aaa";
                }
                return abc.def.ghi.jkl;
            )
        "#;
        assert_infer_err!(
            input,
            create_conflict_type_return_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::String)),
            )
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
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );

        let input = r#"
            let abc = {
              def: {
                ghi: fn() {
                  return 123
                },
                jkl: {
                  mno: fn() {
                    return "pqr";
                  }
                }
              }
            } in (
                abc.def.ghi() + abc.def.jkl.mno();
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
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
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            let abc = 123 in (
                abc.nothing;
            )
        "#;
        assert_infer_err!(
            input,
            create_undefined_field_err(&Id(String::from("abc")), &Id(String::from("nothing")),)
        );
    }

    #[test]
    fn maybe_failed() {
        let input = r#"
            let abc = [1, 2, 3] in (
              abc + 234;
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Array(ArrayType::Defined(Box::new(
                    PrimitiveType::Int
                ))),),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
        );
    }

    #[test]
    fn return_polymophism() {
        let input = r#"
            let abc = fn(a) {
              return a;
            } in (
              "def" + abc("def");
              abc(3) + "abc";
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );

        let input = r#"
            let abc = fn(a) {
              return a;
            } in (
                let def = fn(b) {
                  return b;
                } in (
                  abc(1) + def("2");
                )
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );
    }
}
