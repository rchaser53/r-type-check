use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::infer::*;
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

pub fn create_infered_other_type_err(id: &Id, defined: &TypeKind, new: &TypeKind) -> String {
    format!(
        "{:?} is assined two type. defined:{:?} new:{:?}",
        id, defined, new
    )
}

pub fn create_conflict_type_return_err(defined: &TypeResult, new: &TypeResult) -> String {
    format!("return some types. defined:{:?} new:{:?}", defined, new)
}

pub fn create_cannot_call_err(id: &Id) -> String {
    format!("cannot call {:?}", id)
}
