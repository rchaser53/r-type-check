use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{easy, parser, position};

use crate::expr::uni::*;

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

parser! {
   pub fn uni['a]()(MyStream<'a>) -> Uni
    {
        uni_()
        .and(position())
        .map(|(name, _pos): (Uni, SourcePosition)|{
            name
        })
    }
}
