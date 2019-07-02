use std::sync::Mutex;

use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{token, Parser};

use crate::error::*;
use crate::expr::uni::*;

pub fn skip_spaces<I, T>(
    input: impl Parser<Input = I, Output = T>,
) -> impl Parser<Input = I, Output = T>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let spaces_ = || spaces().silent();
    input.skip(spaces_())
}

pub fn token_skip_spaces<I>(c: char) -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(token(c))
}

pub fn string_skip_spaces<I>(input: &'static str) -> impl Parser<Input = I, Output = &'static str>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(string(input))
}

pub struct Pool(Mutex<i64>);
impl Pool {
    pub fn next_id(&self) -> Id {
        let mut temp = self.0.lock().unwrap();
        *temp += 1;
        Id(temp.to_string())
    }

    pub fn new() -> Pool {
        Pool(Mutex::new(0))
    }
}

pub struct ErrorStack(Mutex<Vec<TypeError>>);
impl ErrorStack {
    pub fn new() -> ErrorStack {
        ErrorStack(Mutex::new(vec![]))
    }

    pub fn push(&self, error: TypeError) {
        let mut temp = self.0.lock().unwrap();
        temp.push(error);
    }

    pub fn len(&self) -> usize {
        let temp = self.0.lock().unwrap();
        temp.len()
    }

    pub fn emit(&self) -> Vec<TypeError> {
        self.0.lock().unwrap().to_vec()
    }
}

lazy_static! {
    pub static ref ID_POOL: Pool = { Pool::new() };
    pub static ref ERROR_STACK: ErrorStack = { ErrorStack::new() };
}

#[macro_export]
macro_rules! DEBUG_INFO {
    ($name:expr, $($exp:expr),*) => {
        if let Ok(word) = env::var("DEBUG_INFO") {
            if word == "1" {
                println!("\n{}", $name);
                $(
                  dbg!($exp);
                )*
            }
        }
    };
}
