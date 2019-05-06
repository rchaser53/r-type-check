use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, Parser};

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{integer, Uni};

#[derive(Debug, PartialEq)]
pub enum Expr {
    BinOp(Box<Uni>, BinOpKind, Box<Uni>),
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // let skip_spaces = || spaces().silent();
    binary()
}

pub fn binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
      (
        integer(),
        bin_op_(),
        integer()
      ).map(|(left, op, right)| Expr::BinOp(Box::new(left), op, Box::new(right)))
}

parser! {
    pub fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}
