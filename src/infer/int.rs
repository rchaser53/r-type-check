use crate::error::*;
use crate::infer::*;

pub fn resolve_int_method(parent_id: &Id, id: &Id, _context: &Context) -> Result<TypeResult> {
    match id.0.as_str() {
        "length" => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::Int,
        ))),
        "to_string" => Ok(TypeResult::Resolved(TypeKind::PrimitiveType(
            PrimitiveType::String,
        ))),
        _ => Err(create_undefined_field_err(parent_id, id)),
    }
}
