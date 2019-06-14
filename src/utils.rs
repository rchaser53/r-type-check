use std::sync::Mutex;

use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{token, Parser};

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

lazy_static! {
    pub static ref ID_POOL: Pool = {
        let mut pool = Pool::new();
        pool
    };
}

#[macro_export]
macro_rules! DEBUG_INFO {
    ($exp:expr) => {
        if let Ok(word) = env::var("DEBUG_INFO") {
            if word == "1" {
                dbg!($exp);
            }
        }
    };
}
