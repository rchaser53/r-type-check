use crate::expr::bin_op::*;
use crate::expr::uni::*;
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

pub fn create_not_initialized_err(left: &Id) -> String {
    format!("{:?} is not initialized", left)
}
