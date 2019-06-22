use combine::char::spaces;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{attempt, choice, easy, many, parser, position};

use crate::expr::uni::*;
use crate::expr::*;
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

parser! {
   pub fn statement_pos['a]()(MyStream<'a>) -> Statement
    {
        choice(
            (
                attempt(return_()),
                attempt(if_()),
                attempt(let_()),
                attempt(assign()),
                attempt(expr_statement())
            )
        )
        .and(position())
        .map(|(name, pos): (Statement, SourcePosition)|{
            name
        })
    }
}

parser! {
   pub fn ast['a]()(MyStream<'a>) -> Vec<Statement>
    {
        spaces().with(many(statement()))
        .map(|statements| statements)
    }
}

mod test {
    use combine::Parser;
    use combine::stream::state::{State};
    use crate::expr::uni::*;
    use crate::pos::*;

    #[test]
    fn test_pos_parser_test() {
        assert_eq!(
            uni().easy_parse(State::new(r#"[
              123,
              456,
              789
            ]"#),),
            Ok((Uni::Id(Id(String::from("abc1"))), State::new(r#""#)))
        );
    }
}