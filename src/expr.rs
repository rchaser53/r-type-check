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
    BinOp(Uni, BinOpKind, Uni),
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
    integer()
        .skip(spaces())
        .and(bin_op_())
        .skip(spaces())
        .and(integer())
        .skip(spaces())
        .map(|((left, op), right)| Expr::BinOp(left, op, right))
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
    fn binary_test() {
        assert_eq!(
            expr().parse(r#"123 + 456"#),
            Ok((
                Expr::BinOp(Uni::Number(123), BinOpKind::Add, Uni::Number(456),),
                ""
            ))
        );
    }
}
