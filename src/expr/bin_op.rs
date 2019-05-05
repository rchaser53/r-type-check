use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{choice, many1, parser, Parser};

use combine::parser::item::any;

#[derive(Debug, PartialEq)]
pub enum BinOpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Shr, // %
    Eq,  // ==
    Lt,  // <
    Le,  // <=
    Ne,  // !=
    Ge,  // >=
    Gt,  // >
}

pub fn bin_op_<I>() -> impl Parser<Input = I, Output = BinOpKind>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (op())
}

pub fn op<I>() -> impl Parser<Input = I, Output = BinOpKind>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("+").map(|_| BinOpKind::Add)
}

parser! {
    pub fn bin_op[I]()(I) -> BinOpKind
    where [I: Stream<Item = char>]
    {
        bin_op_()
    }
}

mod test {
    use crate::expr::bin_op::*;

    #[test]
    fn bin_op_test() {
        assert_eq!(bin_op().parse(r#"+"#), Ok((BinOpKind::Add, "")));
    }
}
