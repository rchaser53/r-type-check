#![recursion_limit = "256"]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate combine;

pub mod ast;
pub mod error;
pub mod expr;
pub mod infer;
pub mod pos;
pub mod scope;
pub mod statement;
pub mod types;
pub mod utils;

pub mod new_error;
pub mod new_expr;
pub mod new_scope;
pub mod new_statement;
pub mod new_types;
pub mod new_utils;

fn main() {}
