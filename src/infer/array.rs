use crate::error::*;
use crate::infer::*;

pub fn resolve_array_method(parent_id: &Id, id: &Id, context: &Context) -> Result<TypeResult> {
    match id.0.as_str() {
        "length" => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Int,
        ))),
        "push" => Ok(TypeResult::Resolved(TypeKind::Function(
            parent_id.clone(),
            vec![OpeaqueType::Unknown],
            OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(PrimitiveType::Void))),
        ))),
        "pop" => {
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
                    ArrayType::Defined(type_kind) => Ok(TypeResult::Resolved(TypeKind::Function(
                        parent_id.clone(),
                        vec![],
                        OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(*type_kind))),
                    ))),
                    _ => unimplemented!(),
                }
            } else {
                Ok(TypeResult::Unknown)
            }
        }
        _ => Err(create_undefined_field_err(parent_id, id)),
    }
}
