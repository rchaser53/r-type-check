use crate::error::*;
use crate::infer::*;

pub fn resolve_array_method(
    parent_id: &Id,
    id: &Id,
    first_call_arg_type: Option<TypeKind>,
    context: &Context,
) -> Result<TypeResult> {
    match id.0.as_str() {
        "length" => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Int,
        ))),
        "push" => {
            match get_array_type(parent_id, context) {
                TypeResult::Resolved(TypeKind::Function(_, _, return_type)) => {
                    if let Some(first_arg_type_kind) = first_call_arg_type {
                        if let OpeaqueType::Defined(type_kind) = return_type {
                            if first_arg_type_kind != *type_kind {
                                return Err(create_mismatch_element_err(
                                    &type_kind,
                                    &first_arg_type_kind,
                                ));
                            }
                        }
                    }
                }
                _ => {}
            };

            Ok(TypeResult::Resolved(TypeKind::Function(
                parent_id.clone(),
                vec![OpeaqueType::Unknown],
                OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(PrimitiveType::Void))),
            )))
        }
        "pop" => Ok(get_array_type(parent_id, context)),
        _ => Err(create_undefined_field_err(parent_id, id)),
    }
}

fn get_array_type(parent_id: &Id, context: &Context) -> TypeResult {
    let result = context
        .scope
        .type_map
        .borrow_mut()
        .try_get(parent_id)
        .and_then(|type_result| match type_result {
            TypeResult::Resolved(TypeKind::PrimitiveType(PrimitiveType::Array(array))) => {
                Some(array.clone())
            }
            _ => unimplemented!(),
        });
    if let Some(result) = result {
        match result {
            ArrayType::Defined(type_kind) => TypeResult::Resolved(TypeKind::Function(
                parent_id.clone(),
                vec![],
                OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(*type_kind))),
            )),
            _ => unimplemented!(),
        }
    } else {
        TypeResult::Unknown
    }
}
