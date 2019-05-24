use combine::error::ParseError;
use combine::parser::char::spaces;
use combine::stream::Stream;
use combine::{attempt, many, parser, sep_by, token, Parser};

use crate::expr::uni::*;
use crate::expr::*;

use crate::statement::*;

#[derive(Debug, PartialEq)]
pub struct Interface(Id, Expr);