use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{easy, parser, position};

use crate::expr::uni::*;

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

parser! {
   fn test_pos_parser['a]()(MyStream<'a>) -> Uni
    {
        uni_()
        .and(position())
        .map(|(name, pos): (Uni, SourcePosition)|{
            dbg!(&pos);
            name
        })
    }
}
