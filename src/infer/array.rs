use crate::error::*;
use crate::infer::*;

pub fn resolve_array_method(
    parent_id: &Id,
    id: &Id,
    _context: &Context,
) -> Result<TypeResult> {
    match id.0.as_str() {
        "length" => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Int,
        ))),
        "push" => Ok(TypeResult::Resolved(TypeKind::Function(
            parent_id.clone(),
            vec![OpeaqueType::Unknown],
            OpeaqueType::Defined(Box::new(TypeKind::PrimitiveType(PrimitiveType::Void))),
        ))),
        _ => Err(create_undefined_field_err(parent_id, id)),
    }
}
