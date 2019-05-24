use combine::error::ParseError;
use combine::parser::char::spaces;
use combine::stream::Stream;
use combine::Parser;

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
