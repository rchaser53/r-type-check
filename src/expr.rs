use combine::error::ParseError;
use combine::parser::char::spaces;
use combine::stream::Stream;
use combine::{attempt, many, parser, token, Parser};

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{uni as create_uni, Uni};

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
    try_paren_with_binary()
}

pub fn unary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    create_uni().map(Expr::Unary)
}

pub fn try_paren_with_binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(attempt(
        token('(')
            .skip(spaces())
            .and(try_binary())
            .skip(spaces())
            .and(token(')'))
            .skip(spaces())
            .map(|((_, exp), _)| exp)
            .skip(spaces())
            .and(many(bin_op_().skip(spaces()).and(expr())))
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
            }),
    ))
    .or(try_binary())
}

pub fn try_binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(
        unary()
            .skip(spaces())
            .and(many(bin_op_().skip(spaces()).and(expr())))
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
            }),
    )
    .or(unary())
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
    fn add_and_mul() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    ))
                ),
                ""
            ))
        );
    }

    #[test]
    fn paren() {
        assert_eq!(
            expr().easy_parse(r#"(1 + 2 * 3)"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    ))
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"1 + (2 * 3)"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    ))
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"(1 + 2) * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Add,
                        Box::new(Expr::Unary(Uni::Number(2))),
                    )),
                    BinOpKind::Mul,
                    Box::new(Expr::Unary(Uni::Number(3))),
                ),
                ""
            ))
        );
    }
}
