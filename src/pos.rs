use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{attempt, choice, easy, many, parser, position};

use crate::expr::uni::*;
use crate::statement::*;

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

parser! {
   pub fn uni['a]()(MyStream<'a>) -> Uni
    {
        uni_()
        .and(position())
        .map(|(name, pos): (Uni, SourcePosition)|{
            name
        })
    }
}

// mod test {
//     use crate::pos::*;
//     use combine::stream::state::State;
//     use combine::Parser;

//     #[test]
//     fn test_pos_parser_test() {
//         assert_eq!(
//             uni().easy_parse(State::new(
//                 r#"[
//               123,
//               456,
//               789
//             ]"#
//             ),),
//             Ok((Uni::Id(Id(String::from("abc1"))), State::new(r#""#)))
//         );
//     }
// }
