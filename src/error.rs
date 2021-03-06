use std::fmt;

use combine::stream::state::SourcePosition;

use crate::expr::bin_op::*;
use crate::expr::uni::*;
use crate::scope::*;
use crate::types::*;

#[derive(Clone, PartialEq)]
pub struct TypeError {
    pub message: String,
    pos: SourcePosition,
}

impl TypeError {
    pub fn new(message: String) -> TypeError {
        TypeError {
            message,
            pos: Default::default(),
        }
    }

    pub fn set_pos(&mut self, pos: SourcePosition) {
        self.pos = pos;
    }

    pub fn create_message(&self) -> String {
        format!("L:{:?} {:?}", self.pos.line, self.message)
    }
}

impl fmt::Debug for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L:{} {}", self.pos.line, self.message)
    }
}

pub fn create_type_mismatch_err(left: &TypeKind, right: &TypeKind) -> TypeError {
    TypeError::new(format!(
        "type is mismatch: left:{:?} right:{:?}",
        left, right
    ))
}

pub fn create_cannot_use_op_err(left: &TypeKind, op: BinOpKind, right: &TypeKind) -> TypeError {
    TypeError::new(format!(
        "cannot use op: left:{:?} op:{:?} right:{:?}",
        left, op, right
    ))
}

pub fn create_cannot_use_op_one_side_err(oneside: &TypeKind, op: BinOpKind) -> TypeError {
    TypeError::new(format!("cannot use op: op:{:?} for type:{:?}", op, oneside))
}

pub fn create_not_initialized_err(left: &Id) -> TypeError {
    TypeError::new(format!("{:?} is not initialized", left))
}

pub fn create_assign_conflict_type_err(id: &Id, defined: &TypeKind, new: &TypeKind) -> TypeError {
    TypeError::new(format!(
        "{:?} is assined two type. defined:{:?} new:{:?}",
        id, defined, new
    ))
}

pub fn create_conflict_type_return_err(defined: &TypeResult, new: &TypeResult) -> TypeError {
    TypeError::new(format!(
        "return some types. defined:{:?} new:{:?}",
        defined, new
    ))
}

pub fn create_cannot_call_err(id: &Id, type_kind: &TypeKind) -> TypeError {
    TypeError::new(format!("cannot call {:?} type:{:?}", id, type_kind))
}

pub fn create_param_and_arg_type_is_mismatch_err(
    arg: &TypeResult,
    param: &TypeResult,
) -> TypeError {
    TypeError::new(format!(
        "param and arg type is mismatch arg type:{:?} param type:{:?}",
        arg, param
    ))
}

pub fn create_if_condition_not_boolean_err(type_kind: &TypeKind) -> TypeError {
    TypeError::new(format!(
        "if condition should be boolean but param type:{:?}",
        type_kind
    ))
}

pub fn create_conflict_array_elemenet_type_err(
    one_type_result: &TypeResult,
    two_type_result: &TypeResult,
) -> TypeError {
    TypeError::new(format!(
        "some array element types are found ex. {:?}, {:?}",
        one_type_result, two_type_result
    ))
}

pub fn create_undefined_field_err(parent_id: &Id, current_id: &Id) -> TypeError {
    TypeError::new(format!("{:?} is undefined in ${:?}", current_id, parent_id))
}

pub fn create_mismatch_element_err(
    base_type_kind: &TypeKind,
    pushed_type_kind: &TypeKind,
) -> TypeError {
    TypeError::new(format!(
        "array type is {:?}. but the type tryed to set is ${:?}",
        base_type_kind, pushed_type_kind
    ))
}

pub fn create_cannnot_assign(id: &Id, wrong_type: &TypeResult) -> TypeError {
    TypeError::new(format!(
        "{:?} is cannot be indexed. type: {:?}",
        id, wrong_type
    ))
}

pub fn create_hash_mismatch_err(id: &Id, defined: &TypeMap, new: &TypeMap) -> TypeError {
    TypeError::new(format!(
        "{:?} is used two type. defined:{:?} new:{:?}",
        id, defined, new
    ))
}

pub fn create_arg_length_is_not_match(
    id: &Id,
    arg_length: usize,
    parameter_length: usize,
) -> TypeError {
    TypeError::new(format!(
        "{:?} argument length is {}. parameter length is {}",
        id, arg_length, parameter_length
    ))
}
