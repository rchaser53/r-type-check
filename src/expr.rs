use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, Parser};

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{integer, uni as create_uni, Uni};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Uni),
    Binary(Uni, BinOpKind, Uni),
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    choice((binary(), unary())).skip(skip_spaces())
}

pub fn unary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    create_uni().map(Expr::Unary)
}

pub fn binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    integer()
        .skip(spaces())
        .and(bin_op_())
        .skip(spaces())
        .and(integer())
        .skip(spaces())
        .map(|((left, op), right)| Expr::Binary(left, op, right))
}

parser! {
    pub fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}

mod test {
    use crate::expr::bin_op::*;
    use crate::expr::uni::*;
    use crate::expr::*;

    #[test]
    fn unary_test() {
        assert_eq!(
            expr().easy_parse(r#""abc""#),
            Ok((Expr::Unary(Uni::String(String::from("abc"))), ""))
        );
    }
}
