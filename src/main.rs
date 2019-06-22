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

fn main() {}
