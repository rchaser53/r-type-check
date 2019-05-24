use combine::error::ParseError;
use combine::parser::char::spaces;
use combine::stream::Stream;
use combine::{token, Parser};

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
