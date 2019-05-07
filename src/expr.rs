use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{between, choice, many, many1, parser, sep_by, Parser};
use std::mem;

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{integer, uni as create_uni, Uni};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Uni),
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // let skip_spaces = || spaces().silent();
    // choice((binary(), unary())).skip(skip_spaces())
    binary()
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
    unary()
        .skip(spaces())
        .and(many(bin_op_().skip(spaces()).and(unary())))
        .skip(spaces())
        .map(|(left, mut others): (Expr, Vec<(BinOpKind, Expr)>)| {
            let length = others.len();
            if length == 0 {
                return left;
            }

            let mut exp = left;
            for _ in 0..length {
                let (bin_op, right) = others.swap_remove(0);
                exp = Expr::Binary(Box::new(exp), bin_op, Box::new(right));
            }

            exp
        })
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

    #[test]
    fn add_test() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(2))),
                ),
                ""
            ))
        );
    }

    #[test]
    fn add_and_() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Add,
                        Box::new(Expr::Unary(Uni::Number(2)))
                    )),
                    BinOpKind::Mul,
                    Box::new(Expr::Unary(Uni::Number(3))),
                ),
                ""
            ))
        );
    }
}
