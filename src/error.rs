use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::scope::*;
use crate::types::*;

pub fn create_type_mismatch_err(left: &TypeKind, right: &TypeKind) -> String {
    format!("type is mismatch: left:{:?} right:{:?}", left, right)
}

pub fn create_cannot_use_op_err(left: &TypeKind, op: BinOpKind, right: &TypeKind) -> String {
    format!(
        "cannot use op: left:{:?} op:{:?} right:{:?}",
        left, op, right
    )
}

pub fn create_cannot_use_op_one_side_err(oneside: &TypeKind, op: BinOpKind) -> String {
    format!("cannot use op: op:{:?} for type:{:?}", op, oneside)
}

pub fn create_not_initialized_err(left: &Id) -> String {
    format!("{:?} is not initialized", left)
}

pub fn create_assign_conflict_type_err(id: &Id, defined: &TypeKind, new: &TypeKind) -> String {
    format!(
        "{:?} is assined two type. defined:{:?} new:{:?}",
        id, defined, new
    )
}

pub fn create_conflict_type_return_err(defined: &TypeResult, new: &TypeResult) -> String {
    format!("return some types. defined:{:?} new:{:?}", defined, new)
}

pub fn create_cannot_call_err(id: &Id, type_kind: &TypeKind) -> String {
    format!("cannot call {:?} type:{:?}", id, type_kind)
}

pub fn create_param_and_arg_type_is_mismatch_err(arg: &TypeResult, param: &TypeResult) -> String {
    format!(
        "param and arg type is mismatch arg type:{:?} param type:{:?}",
        arg, param
    )
}

pub fn create_if_condition_not_boolean_err(type_kind: &TypeKind) -> String {
    format!(
        "if condition should be boolean but param type:{:?}",
        type_kind
    )
}

pub fn create_conflict_array_elemenet_type_err(
    one_type_result: &TypeResult,
    two_type_result: &TypeResult,
) -> String {
    format!(
        "some array element types are found ex. {:?}, {:?}",
        one_type_result, two_type_result
    )
}

pub fn create_undefined_field_err(parent_id: &Id, current_id: &Id) -> String {
    format!("{:?} is undefined in ${:?}", current_id, parent_id)
}
