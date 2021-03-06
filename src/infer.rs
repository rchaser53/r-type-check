use combine::stream::state::SourcePosition;
use std::cell::RefCell;
use std::collections::HashMap;
use std::env;

use crate::error::*;
use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::expr::*;
use crate::scope::*;
use crate::statement::*;
use crate::types::*;
use crate::utils::*;

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
    pub current_left_id: RefCell<Option<Id>>,
    pub current_called_id: RefCell<Option<Id>>,
    pub called_map: RefCell<HashMap<Field, TypeResult>>,
    pub scope: LocalScope,
}
impl Context {
    pub fn new() -> Self {
        Context {
            current_left_id: RefCell::new(None),
            current_called_id: RefCell::new(None),
            called_map: RefCell::new(HashMap::new()),
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
                resolve_let_statement(lets, body, context)?;
            }
            StmtKind::Assign(Assign(accesiable, expr)) => {
                resolve_assign_statement(accesiable, expr, context)?;
            }
            StmtKind::Return(expr) => {
                resolve_return_statement(expr, &mut return_type_results, context)?;
            }
            StmtKind::If(if_tuples) => {
                resolve_if_statement(if_tuples, &mut return_type_results, context)?;
            }
            StmtKind::Expr(expr) => {
                resolve_expr(expr, context)?;
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
                ERROR_STACK.push(err.clone());
                return Err(err);
            }
        }
        Ok(first_type_result)
    }
}

fn resolve_let_statement(
    lets: Vec<Assign>,
    body: Vec<Statement>,
    context: &Context,
) -> Result<TypeResult> {
    for Assign(assignable, exp) in lets {
        // TBD need to think more
        match assignable {
            Accessiable::Field(field) => {
                context.current_left_id.replace(Some(field.id.0.clone()));
                let right_type_result = resolve_expr(exp.clone(), &context)?;

                if let Node::Fn(function) = exp.node {
                    let Function(args, body) = function.clone();
                    let fn_context = context.clone();
                    // field.id.0 is not needed maybe
                    // for call recursive
                    fn_context.scope.type_map.borrow_mut().insert(
                        field.id.0.clone(),
                        TypeResult::Resolved(TypeKind::Function(FunctionType(
                            field.id.0.clone(),
                            args.clone()
                                .into_iter()
                                .map(|id| OpeaqueType::IdOnly(id))
                                .collect(),
                            OpeaqueType::Unknown,
                        ))),
                    );
                    if let TypeResult::Resolved(TypeKind::Function(function_type)) =
                        resolve_fn(field.id.0.clone(), args, body, &fn_context)?
                    {
                        context
                            .scope
                            .function_map
                            .borrow_mut()
                            .insert(field.id.0.clone(), function_type);
                    }
                };

                context
                    .scope
                    .type_map
                    .borrow_mut()
                    .try_insert(field.id.0, right_type_result)?;
            }
            _ => unreachable!(),
        }
    }
    context.current_left_id.replace(None);
    match resolve_statement(body, context) {
        Ok(result) => Ok(result),
        Err(err) => {
            ERROR_STACK.push(err.clone());
            Err(err)
        }
    }
}

fn resolve_assign_statement(
    accessiable: Accessiable,
    expr: Expr,
    context: &Context,
) -> Result<TypeResult> {
    match accessiable {
        Accessiable::Field(field) => {
            context.current_left_id.replace(Some(field.id.0.clone()));
            let result = resolve_assign_field(field, expr, context);
            context.current_left_id.replace(None);
            result
        }
        Accessiable::Index(Index(field, indexes)) => {
            context.current_left_id.replace(Some(field.id.0.clone()));
            let result = resolve_assign_index(field, indexes, expr, context);
            context.current_left_id.replace(None);
            result
        }
    }
}

fn resolve_assign_field(field: Field, exp: Expr, context: &Context) -> Result<TypeResult> {
    let position = exp.position.lo;
    let right_type_result = resolve_expr(exp.clone(), context)?;
    if let Some(left_type) = context.scope.type_map.borrow_mut().try_get(&field.id.0) {
        if let TypeResult::Resolved(left_type) = left_type {
            if let Some(mut err) = validate_assign_type(left_type, &right_type_result) {
                err.set_pos(position);
                return Err(err);
            }
        };
    } else {
        let mut err = create_not_initialized_err(&field.id.0);
        err.set_pos(position);
        return Err(err);
    };
    context
        .scope
        .type_map
        .borrow_mut()
        .try_insert(field.id.0, right_type_result)
}

// TBD need to think more
fn resolve_assign_index(
    field: Field,
    _indexes: Vec<usize>,
    exp: Expr,
    context: &Context,
) -> Result<TypeResult> {
    let position = exp.position.lo;
    let right_type_result = resolve_expr(exp, context)?;
    if let Some(left_type) = context.scope.type_map.borrow_mut().try_get(&field.id.0) {
        if let TypeResult::Resolved(left_type) = left_type {
            if let TypeKind::PrimitiveType(PrimitiveType::Array(array_type)) = left_type {
                if let ArrayType::Defined(boxed_primitive_type) = array_type {
                    if let Some(mut err) = validate_assign_type(
                        &TypeKind::PrimitiveType(*boxed_primitive_type.clone()),
                        &right_type_result,
                    ) {
                        err.set_pos(position);
                        return Err(err);
                    }
                }
            }
        };
    } else {
        let mut err = create_not_initialized_err(&field.id.0);
        err.set_pos(position);
        return Err(err);
    };
    context
        .scope
        .type_map
        .borrow_mut()
        .try_insert(field.id.0, right_type_result)
}

fn validate_assign_type(left_type: &TypeKind, right_type_result: &TypeResult) -> Option<TypeError> {
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

fn resolve_return_statement(
    expr: Expr,
    return_type_results: &mut Vec<(TypeResult, SourcePosition)>,
    context: &Context,
) -> Result<TypeResult> {
    let position = expr.position.lo;
    let type_result = resolve_expr(expr, context)?;
    return_type_results.push((type_result.clone(), position));
    Ok(type_result)
}

fn resolve_if_statement(
    if_tuples: Vec<(Expr, Vec<Statement>)>,
    return_type_results: &mut Vec<(TypeResult, SourcePosition)>,
    context: &Context,
) -> Result<TypeResult> {
    let mut if_return_types = vec![];
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
                    TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
                )?;
            }
            _ => unreachable!(),
        };
        let unboxed_body = boxed_body;
        // get return type in if statement
        match resolve_statement(unboxed_body, context) {
            Ok(result) => {
                match result {
                    TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void)) => {}
                    result => {
                        if_return_types.push((result, position));
                    }
                };
            }
            Err(err) => {
                ERROR_STACK.push(err.clone());
                return Err(err);
            }
        };
    }

    let result_type = if if_return_types.is_empty() {
        TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
    } else {
        if_return_types.first().unwrap().0.clone()
    };
    return_type_results.append(&mut if_return_types);

    Ok(result_type)
}

pub fn resolve_expr(exp: Expr, context: &Context) -> Result<TypeResult> {
    let exp_id = exp.id.clone();
    let position = exp.position.lo;
    let result = match exp.node {
        Node::Unary(uni) => resolve_uni(uni, context),
        Node::Binary(left, op, right) => resolve_binary(*left, op, *right, context),
        Node::Fn(Function(args, body)) => resolve_fn(exp_id, args, body, context),
        Node::Call(accessiable, boxed_args) => {
            resolve_call(exp_id, accessiable, boxed_args, context)
        }
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
    context: &Context,
) -> Result<TypeResult> {
    let fn_context = context.clone();
    for arg in args.clone() {
        fn_context
            .scope
            .type_map
            .borrow_mut()
            .insert(arg.clone(), TypeResult::IdOnly(arg));
    }

    let result = match resolve_statement(body, &fn_context) {
        Ok(result) => result,
        Err(err) => {
            ERROR_STACK.push(err.clone());
            return Err(err);
        }
    };

    // insert result to context in the case of object
    if let TypeResult::Resolved(TypeKind::Scope(ref id_type)) = result {
        let fn_scope = fn_context.scope.scope_map.borrow_mut();
        context
            .scope
            .scope_map
            .borrow_mut()
            .insert(id_type.clone(), fn_scope.get(id_type).unwrap().clone());
    }

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
    Ok(TypeResult::Resolved(TypeKind::Function(FunctionType(
        id,
        fn_arg_types,
        return_type,
    ))))
}

pub fn resolve_call(
    id: Id,
    accessiable: Accessiable,
    args: Vec<Expr>,
    context: &Context,
) -> Result<TypeResult> {
    let field = match accessiable {
        Accessiable::Field(field) => field,
        Accessiable::Index(Index(field, _)) => field,
    };

    // check call recursively
    let current_call_id = context.current_called_id.borrow_mut().clone();
    let called_same_fn = Some(id.clone()) == current_call_id;
    if let Some(type_result) = context.called_map.borrow_mut().get(&field) {
        if called_same_fn {
            // doesn't need to initialize current_called_id
            // because call recursively
            return Ok(type_result.clone());
        }
    }

    let temp_called_id = context.current_called_id.borrow_mut().clone();
    context.current_called_id.replace(Some(id));

    let first_type = if let Some(exp) = args.first() {
        match resolve_expr(exp.clone(), context)? {
            TypeResult::Resolved(type_kind) => Some(type_kind),
            _ => None,
        }
    } else {
        None
    };
    let id = get_id(field.id.clone(), field.child.clone()).0;
    let type_result = resolve_field(field.clone(), vec![], first_type, context)?;

    let arg_len = args.len();
    let mut arg_type_vec = Vec::with_capacity(arg_len);

    let field_type_result = match type_result {
        TypeResult::IdOnly(_) => {
            for item in arg_type_vec.iter_mut().take(arg_len) {
                *item = OpeaqueType::Unknown
            }

            let type_result = TypeResult::Resolved(TypeKind::Function(FunctionType(
                id.clone(),
                arg_type_vec,
                OpeaqueType::Unknown,
            )));

            context
                .scope
                .type_map
                .borrow_mut()
                .insert(id.clone(), type_result.clone());

            type_result
        }
        type_result => type_result,
    };

    let fn_context = context.clone();
    let result = match field_type_result {
        TypeResult::Resolved(TypeKind::Function(FunctionType(_, params, return_opeaque))) => {
            let params_length = params.len();
            for (index, param) in params.into_iter().enumerate() {
                match param {
                    OpeaqueType::Defined(param_type_kind) => {
                        let arg_exp = if let Some(arg_exp) = args.get(index) {
                            arg_exp
                        } else {
                            context.current_called_id.replace(temp_called_id);
                            return Err(create_arg_length_is_not_match(
                                &id,
                                args.len(),
                                params_length,
                            ));
                        };
                        let arg_type_result = resolve_expr(arg_exp.clone(), &fn_context)?;
                        let param_type_result = TypeResult::Resolved(*param_type_kind.clone());
                        if arg_type_result != param_type_result {
                            context.current_called_id.replace(temp_called_id);
                            return Err(create_param_and_arg_type_is_mismatch_err(
                                &arg_type_result,
                                &param_type_result,
                            ));
                        }
                    }
                    OpeaqueType::IdOnly(arg_id) => {
                        let arg_exp = if let Some(arg_exp) = args.get(index) {
                            arg_exp
                        } else {
                            context.current_called_id.replace(temp_called_id);
                            return Err(create_arg_length_is_not_match(
                                &id,
                                args.len(),
                                params_length,
                            ));
                        };
                        let arg_type_result = resolve_expr(arg_exp.clone(), &fn_context)?;
                        // save type information for paramerter in function
                        // and retry type check somewhere
                        fn_context
                            .scope
                            .type_map
                            .borrow_mut()
                            .insert(arg_id, arg_type_result);
                    }
                    _ => {
                        // TBD: need to check correctly
                    }
                }
            }
            Ok(return_opeaque.convert_type_result())
        }
        TypeResult::Resolved(type_kind) => Err(create_cannot_call_err(&id, &type_kind)),
        TypeResult::IdOnly(_) | TypeResult::Unknown => Ok(TypeResult::Unknown),
        _ => unreachable!(),
    }?;

    let mut fn_map = context.scope.function_map.borrow_mut();
    let fn_return_type_result = if let Some(FunctionType(_, arg_types, return_type)) =
        fn_map.get_mut(&id)
    {
        if args.len() != arg_types.len() {
            let err = create_arg_length_is_not_match(&id, args.len(), arg_types.len());
            ERROR_STACK.push(err.clone());
            return Err(err);
        }

        match return_type {
            OpeaqueType::Unknown => TypeResult::Unknown,
            OpeaqueType::IdOnly(id) => TypeResult::IdOnly(id.clone()),
            OpeaqueType::Defined(boxed_type_kind) => TypeResult::Resolved(*boxed_type_kind.clone()),
        }
    } else {
        return match result {
            TypeResult::IdOnly(_) => Ok(TypeResult::Unknown),
            other => Ok(other),
        };
    };

    context
        .called_map
        .borrow_mut()
        .insert(field, fn_return_type_result.clone());
    context.current_called_id.replace(temp_called_id);
    match (&fn_return_type_result, &result) {
        (TypeResult::Resolved(left_type_kind), TypeResult::Resolved(right_type_kind)) => {
            if !compare_type_kind(left_type_kind.clone(), right_type_kind.clone(), context) {
                return Err(create_conflict_type_return_err(
                    &TypeResult::Resolved(left_type_kind.clone()),
                    &TypeResult::Resolved(right_type_kind.clone()),
                ));
            }

            // when return type is object
            // need to set object_scope to context
            for (key, value) in fn_context.scope.scope_map.borrow_mut().iter() {
                context
                    .scope
                    .scope_map
                    .borrow_mut()
                    .insert(key.clone(), value.clone());
            }

            Ok(fn_return_type_result)
        }
        (TypeResult::IdOnly(id), _) => {
            if let Some(type_result) = fn_context.scope.type_map.borrow_mut().try_get(id) {
                Ok(type_result.clone())
            } else {
                Ok(TypeResult::Unknown)
            }
        }
        _ => Ok(fn_return_type_result),
    }
}

// TBD need to think more
fn compare_type_kind(left: TypeKind, right: TypeKind, context: &Context) -> bool {
    match (left, right) {
        (TypeKind::Scope(IdType::Object(left_id)), TypeKind::Scope(IdType::Object(right_id))) => {
            match (
                fetch_object_scope(left_id, context),
                fetch_object_scope(right_id, context),
            ) {
                (Some(left_scope), Some(right_scope)) => left_scope == right_scope,
                _ => true,
            }
        }
        (left, right) => left == right,
    }
}

pub fn get_id(id: ObjectId, field: Option<Box<Field>>) -> ObjectId {
    if let Some(field) = field {
        get_id(field.id, field.child)
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
    let left_id = left.id;
    let right_id = right.id;
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
            let r_resolved = resolve_call(right_id, r_field, args, context)?;
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
            let r_resolved = resolve_call(right_id, r_field, args, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, args), Node::Binary(r_left, r_op, r_right)) => {
            let l_resolved = resolve_call(left_id, l_field, args, context)?;
            let r_resolved = resolve_binary(*r_left, r_op, *r_right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, args), Node::Unary(right)) => {
            let l_resolved = resolve_call(left_id, l_field, args, context)?;
            let r_resolved = resolve_uni(right, context)?;
            resolve_type_result_with_op(l_resolved, op, r_resolved, context)
        }
        (Node::Call(l_field, left_args), Node::Call(r_field, right_args)) => {
            let l_resolved = resolve_call(left_id, l_field, left_args, context)?;
            let r_resolved = resolve_call(right_id, r_field, right_args, context)?;
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
        Uni::Field(field) => resolve_field(field, vec![], None, context)?,
        Uni::Index(Index(field, indexes)) => resolve_index(field, indexes, context)?,
        Uni::Null => unimplemented!(),
    };
    Ok(result)
}

pub fn resolve_index(field: Field, _indexes: Vec<usize>, context: &Context) -> Result<TypeResult> {
    DEBUG_INFO!("resolve_index", &context);
    let id = get_id(field.id.clone(), field.child.clone()).0;
    let type_result = resolve_field(field, vec![], None, context)?;

    match type_result {
        TypeResult::Resolved(TypeKind::PrimitiveType(ref primitive_type)) => {
            if let PrimitiveType::Array(array_type) = primitive_type {
                match array_type {
                    ArrayType::Defined(primitive_type) => Ok(TypeResult::Resolved(
                        TypeKind::PrimitiveType(*primitive_type.clone()),
                    )),
                    _ => Ok(TypeResult::IdOnly(id)),
                }
            } else {
                Err(create_cannnot_assign(&id, &type_result))
            }
        }
        _ => Err(create_cannnot_assign(&id, &type_result)),
    }
}

/// xxx.yyy comes now. xxx is possibility for every type
pub fn resolve_field(
    field: Field,
    mut field_object_ids: Vec<ObjectId>,
    first_call_arg: Option<TypeKind>,
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
            return resolve_field(*child, field_object_ids, first_call_arg, context);
        } else {
            type_result.unwrap()
        };

        resolve_field_object(*child, current_id.0, type_result, first_call_arg, context)
    } else {
        if let Some(type_result) = context.scope.type_map.borrow_mut().try_get(&current_id.0) {
            return Ok(type_result.clone());
        }

        // TBD: need to implement multi nest
        if field_object_ids.is_empty() {
            return Ok(TypeResult::Unknown);
        }

        let first_id = field_object_ids.remove(0);
        let field_object_ids = field_object_ids;
        let mut result_scope = if let Some(scope) = fetch_object_scope(first_id, context) {
            scope
        } else {
            return Ok(TypeResult::Unknown);
        };

        for resolve_id in field_object_ids {
            let temp_scope =
                if let Some(scope) = result_scope.scope_map.borrow_mut().get(&resolve_id) {
                    scope.clone()
                } else {
                    // scope doesn't find
                    return Ok(TypeResult::Unknown);
                };
            result_scope = *temp_scope;
        }

        let mut type_map = result_scope.type_map.borrow_mut();
        match type_map.try_get(&current_id.0) {
            Some(result) => Ok(result.clone()),
            _ => Ok(TypeResult::IdOnly(current_id.0)),
        }
    }
}

pub fn resolve_field_object(
    field: Field,
    current_id: Id,
    type_result: TypeResult,
    first_call_arg: Option<TypeKind>,
    context: &Context,
) -> Result<TypeResult> {
    match type_result {
        TypeResult::Resolved(TypeKind::Scope(id_type)) => match id_type {
            IdType::Object(object_id) => {
                let type_result = resolve_object(object_id, &field.id.0, context);
                let current_id = field.id.0;
                if let Some(child) = field.child {
                    resolve_field_object(*child, current_id, type_result?, first_call_arg, context)
                } else {
                    type_result
                }
            }
            IdType::Local(_) => unimplemented!(),
        },
        TypeResult::Resolved(type_kind) => {
            // check property for primitive type
            resolve_unique_field(current_id, field.id.0, type_kind, first_call_arg, context)
        }
        type_result => Ok(type_result),
    }
}

/// try to get object scope
pub fn resolve_object(object_scope_id: ObjectId, id: &Id, context: &Context) -> Result<TypeResult> {
    if let Some(scope) = fetch_object_scope(object_scope_id, context) {
        if let Some(type_result) = scope.type_map.borrow_mut().try_get(&id) {
            Ok(type_result.clone())
        } else {
            unimplemented!()
        }
    } else {
        unimplemented!()
    }
}

pub fn resolve_unique_field(
    parent_id: Id,
    id: Id,
    type_kind: TypeKind,
    first_call_arg: Option<TypeKind>,
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
            resolve_array_method(parent_id, id, first_call_arg, context)
        }
        result => Ok(TypeResult::Resolved(result)),
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
        ObjectId(left_id)
    } else {
        // this is a case StmtKind::Expr(Expr::Unary(Uni::Hash))
        // so it's not used. like below
        // { abc: 123 };
        return Ok(TypeResult::Unknown);
    };

    let store_current_left_id = context.current_left_id.borrow_mut().clone();
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

        hash_scope.type_map.borrow_mut().insert(key, type_result);
    }
    context.current_left_id.replace(store_current_left_id);

    if let Some(old) = context.scope.scope_map.borrow_mut().insert(
        IdType::Object(hash_scope_id.clone()),
        Box::new(Scope::Object(hash_scope.clone())),
    ) {
        if let Scope::Object(old_object_scope) = *old {
            if old_object_scope != hash_scope {
                return Err(create_hash_mismatch_err(
                    &hash_scope_id.0,
                    &old_object_scope.type_map.borrow_mut().clone(),
                    &hash_scope.type_map.borrow_mut().clone(),
                ));
            }
        } else {
            unreachable!()
        };
    }
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
    match (left, right) {
        (TypeResult::Resolved(ref left), TypeResult::Resolved(ref right)) => {
            check_left_op_right(left, op, right, context)
        }
        (left @ TypeResult::Resolved(_), TypeResult::IdOnly(id)) => {
            try_insert_and_resolve_op(left, id, op, context)
        }
        (left @ TypeResult::Resolved(_), TypeResult::Unknown) => Ok(left),
        (TypeResult::IdOnly(id), right @ TypeResult::Resolved(_)) => {
            try_insert_and_resolve_op(right, id, op, context)
        }
        (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => {
            let left_result = filter_type_result(left_id, context)?;
            let right_result = filter_type_result(right_id, context)?;

            match (left_result, right_result) {
                (TypeResult::Resolved(left_type), TypeResult::Resolved(_)) => {
                    Ok(TypeResult::Resolved(left_type))
                }
                (left_result @ TypeResult::Resolved(_), TypeResult::IdOnly(right_id)) => {
                    try_insert_and_resolve_op(left_result, right_id, op, context)
                }
                (TypeResult::IdOnly(left_id), right_result @ TypeResult::Resolved(_)) => {
                    try_insert_and_resolve_op(right_result, left_id, op, context)
                }
                (TypeResult::IdOnly(left_id), TypeResult::IdOnly(right_id)) => match op {
                    BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Shr => {
                        context.scope.type_map.borrow_mut().insert(
                            left_id,
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                        );
                        context.scope.type_map.borrow_mut().insert(
                            right_id,
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                        );
                        Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                            PrimitiveType::Int,
                        )))
                    }
                    BinOpKind::Lt | BinOpKind::Le | BinOpKind::Ge | BinOpKind::Gt => {
                        context.scope.type_map.borrow_mut().insert(
                            left_id,
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                        );
                        context.scope.type_map.borrow_mut().insert(
                            right_id,
                            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int)),
                        );
                        Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                            PrimitiveType::Boolean,
                        )))
                    }
                    _ => Ok(TypeResult::IdOnly(left_id)),
                },
                _ => unreachable!(),
            }
        }
        (TypeResult::Unknown, right @ TypeResult::Resolved(_)) => Ok(right),
        (TypeResult::Unknown, _) => Ok(TypeResult::Unknown),
        (_, TypeResult::Unknown) => Ok(TypeResult::Unknown),
        _ => unimplemented!(),
    }
}

pub fn try_insert_and_resolve_op(
    type_result: TypeResult,
    id: Id,
    op: BinOpKind,
    context: &Context,
) -> Result<TypeResult> {
    let type_kind = if let TypeResult::Resolved(type_kind) = &type_result {
        type_kind.clone()
    } else {
        unreachable!()
    };

    context
        .scope
        .type_map
        .borrow_mut()
        .try_insert(id, type_result.clone())?;
    match resolve_op_one_side(&type_kind, op, context) {
        Ok(_) => match op {
            BinOpKind::Eq
            | BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Ne
            | BinOpKind::Ge
            | BinOpKind::Gt => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
                PrimitiveType::Boolean,
            ))),
            _ => Ok(type_result),
        },
        Err(err_str) => Err(err_str),
    }
}

// return Resolved or Unknown only
pub fn filter_type_result(id: Id, context: &Context) -> Result<TypeResult> {
    if let Some(result) = context.scope.type_map.borrow_mut().try_get(&id) {
        return match result {
            TypeResult::Resolved(_) => Ok(result.clone()),
            TypeResult::IdOnly(_) => Ok(TypeResult::IdOnly(id)),
            TypeResult::Unknown => unimplemented!(),
            TypeResult::Binary(_, _, _) => unreachable!(),
        };
    }
    context
        .scope
        .type_map
        .borrow_mut()
        .insert(id.clone(), TypeResult::IdOnly(id.clone()));
    Ok(TypeResult::IdOnly(id))
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

fn fetch_object_scope(object_id: ObjectId, context: &Context) -> Option<ObjectScope> {
    if let Some(scope) = context
        .scope
        .scope_map
        .borrow_mut()
        .get(&IdType::Object(object_id))
    {
        match *scope.clone() {
            Scope::Local(_) => unreachable!(),
            Scope::Object(object_scope) => Some(object_scope),
        }
    } else {
        None
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
                &TypeKind::Function(FunctionType(
                    Id(String::from("whatever")),
                    vec![],
                    OpeaqueType::Unknown
                )),
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

        /* TBD need to fix this implement
         // cannot emit error under current implementation
         abc("a", true);
        */
        // let input = r#"
        //     let abc = fn (def, ghi){ return def + ghi; } in (
        //         abc(2, 1) + 33;
        //         abc("a", "b") + "cde";
        //         abc("a", true);
        //     )
        // "#;
        // assert_infer_err!(
        //     input,
        //     create_type_mismatch_err(
        //         &TypeKind::PrimitiveType(PrimitiveType::String),
        //         &TypeKind::PrimitiveType(PrimitiveType::Boolean),
        //     )
        // );
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

    #[test]
    fn infer_primitive_method() {
        let input = r#"
            let abc = 123 in (
                abc + abc.to_string();
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
            let abc = [123, 456] in (
                abc.push("def");
            )
        "#;
        assert_infer_err!(
            input,
            create_mismatch_element_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String),
            )
        );

        let input = r#"
            let abc = [] in (
                abc.push("def");
                abc.push(123);
            )
        "#;
        assert_infer_err!(
            input,
            create_mismatch_element_err(
                &TypeKind::PrimitiveType(PrimitiveType::String),
                &TypeKind::PrimitiveType(PrimitiveType::Int),
            )
        );
    }

    #[test]
    fn infer_resolve_index() {
        let input = r#"
            let abc = [123, 456] in (
                abc[0] + "aaa";
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
            let abc = [123, 456] in (
                abc[0] = "aaa";
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
    fn let_and_param_infer() {
        let input = r#"
            let abc = fn(b) {
                return 12 + b;
            }
            ghi = false
            in (
              abc(ghi);
            )
        "#;
        assert_infer_err!(
            input,
            create_param_and_arg_type_is_mismatch_err(
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Boolean)),
                &TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Int))
            )
        );
    }

    #[test]
    fn return_hash_infer() {
        let input = r#"
            let abc = fn() {
                return { e: 1 };
            } in (
              let def = abc() in (
                def.e + "abc";
              )
            )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn if_condition_infer() {
        let input = r#"
            let abc = fn(a) {
          if (a == 2) {
            return 1;
          } else {
            return 2;
          }
        } in (
          let def = abc(1) in (
            def + "12";
          )
        )
        "#;
        assert_infer_err!(
            input,
            create_type_mismatch_err(
                &TypeKind::PrimitiveType(PrimitiveType::Int),
                &TypeKind::PrimitiveType(PrimitiveType::String)
            )
        );
    }

    #[test]
    fn arg_length_and_param_length_is_not_match() {
        let input = r#"
            let abc = fn(a) {
              if (a > 10) {
                  abc();
              }
              a = a+1;
            } in (
              abc(0)
            )
        "#;
        assert_infer_err!(
            input,
            create_arg_length_is_not_match(&Id(String::from("abc")), 0, 1)
        );
    }

    #[test]
    fn recursive() {
        let input = r#"
            let abc = fn(a) {
              if (a > 10) {
                  abc(1);
              }
              a = a+1;
            } in (
              abc(0)
            )
        "#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );

        let input = r#"
            let abc = fn(a) {
              if (a > 10) {
                  abc(a);
              }
              a = a+1;
              return a;
            } in (
              abc(0);
            )
        "#;
        assert_infer!(
            input,
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Void))
        );
    }
}
