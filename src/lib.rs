#![recursion_limit = "256"]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate combine;

pub mod ast;
pub mod infer;

pub mod error;
pub mod expr;
pub mod pos;
pub mod scope;
pub mod statement;
pub mod types;
pub mod utils;

use combine::stream::state::State;
use combine::Parser;

use crate::ast::*;
use crate::error::*;
use crate::infer::*;
use crate::scope::*;

pub fn compile(input: &'static str) -> Result<TypeResult, TypeError> {
    let mut context = Context::new();
    match ast().easy_parse(State::new(input)) {
        Ok((statements, _)) => resolve_statement(statements, &mut context),
        Err(err) => panic!(err),
    }
}
