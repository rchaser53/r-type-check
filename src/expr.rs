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
    attempt(handle_op(try_paren_with_binary())).or(try_binary())
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
    token('(')
        .skip(spaces())
        .and(try_binary())
        .skip(spaces())
        .and(token(')'))
        .skip(spaces())
        .map(|((_, exp), _)| exp)
}

pub fn handle_op<I>(
    input: impl Parser<Input = I, Output = Expr>,
) -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    input
        .skip(spaces())
        .and(
            attempt(many(bin_op_().skip(spaces()).and(unary())))
                .or(many(bin_op_().skip(spaces()).and(expr()))),
        )
        .skip(spaces())
        .map(|(left, mut right_pairs): (Expr, Vec<(BinOpKind, Expr)>)| {
            match right_pairs.len() {
                0 => return left,
                1 => {
                    let (bin_op, right) = right_pairs.remove(0);
                    return Expr::Binary(Box::new(left), bin_op, Box::new(right));
                }
                _ => {
                    let mut exp = left;
                    let mut left_pair = right_pairs.remove(0);
                    let mut right_pair = right_pairs.remove(0);
                    let mut length = right_pairs.len();
                    loop {
                        let left_priority = left_pair.0.priority();
                        let right_priority = right_pair.0.priority();

                        // [exp1 op1 exp2] op2 exp3
                        if left_priority > right_priority {
                            let (left_op, left_exp) = left_pair;
                            exp = Expr::Binary(Box::new(exp), left_op, Box::new(left_exp));

                            if length == 0 {
                                let (right_op, right_exp) = right_pair;
                                return Expr::Binary(Box::new(exp), right_op, Box::new(right_exp));
                            }

                            left_pair = right_pair;
                            right_pair = right_pairs.remove(0);
                        }
                        // exp1 op1 [exp2 op2 exp3]
                        else {
                            let (left_op, left_exp) = left_pair;
                            let (right_op, right_exp) = right_pair;
                            left_pair = (
                                left_op,
                                Expr::Binary(Box::new(left_exp), right_op, Box::new(right_exp)),
                            );

                            if length == 0 {
                                let (left_op, left_exp) = left_pair;
                                return Expr::Binary(Box::new(left_exp), left_op, Box::new(exp));
                            }

                            right_pair = right_pairs.remove(0);
                        }
                        length -= 1;
                    }
                }
            }
        })
}

pub fn try_binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(handle_op(unary())).or(unary())
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
    fn three() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    )),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(1))),
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"1 * 2 + 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(2))),
                    )),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(3))),
                ),
                ""
            ))
        );
    }

    #[test]
    fn four() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3 - 5"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Number(2))),
                            BinOpKind::Mul,
                            Box::new(Expr::Unary(Uni::Number(3))),
                        )),
                        BinOpKind::Sub,
                        Box::new(Expr::Unary(Uni::Number(5))),
                    )),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(1))),
                ),
                ""
            ))
        );
    }

    #[test]
    fn paren() {
        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    )),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(1))),
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
