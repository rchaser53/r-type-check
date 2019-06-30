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

use combine::easy;
use combine::stream::state::{SourcePosition, State};
use combine::Parser;

use crate::ast::*;
use crate::infer::*;
use crate::statement::*;

pub fn compile(input: &str) -> Result<(), String> {
    match ast().easy_parse(State::new(input)) {
        Ok(result) => {
            let (statements, _): (Vec<Statement>, State<&str, SourcePosition>) = result;
            match resolve_statement(statements, &Context::new()) {
                Ok(_) => Ok(()),
                Err(err) => Err(format!("{:?}", err)),
            }
        }
        Err(err) => {
            let err: easy::Errors<char, &str, SourcePosition> = err;
            panic!(err.position)
        }
    }
}
