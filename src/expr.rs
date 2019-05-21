use combine::error::ParseError;
use combine::parser::char::spaces;
use combine::stream::Stream;
use combine::{attempt, many, parser, sep_by, token, Parser};

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{uni as create_uni, word_, Uni};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Uni),
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
    Call(Uni, Vec<Box<Expr>>),
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
                                return Expr::Binary(Box::new(exp), left_op, Box::new(left_exp));
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

pub fn call<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    word_()
        .skip(spaces())
        .and(token('('))
        .skip(spaces())
        .and(
            sep_by(expr().skip(spaces()), token(',').skip(spaces()))
                .skip(spaces())
                .map(|exps: Vec<Expr>| exps.into_iter().map(|exp| Box::new(exp)).collect()),
        )
        .skip(spaces())
        .and(token(')'))
        .map(|(((fn_name, _), args), _)| {
            match fn_name {
                Uni::Id(_) | Uni::Field(_) => Expr::Call(fn_name, args),
                _ => panic!("should come Uni::Id. actual: {:?}", fn_name),
            }
        })
}

parser! {
    pub fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        attempt(call())
          .or(expr_())
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
    fn call_test() {
        assert_eq!(
            expr().easy_parse(r#"ab()"#),
            Ok((Expr::Call(Uni::Id(Id(String::from("ab"))), vec![]), ""))
        );

        assert_eq!(
            expr().easy_parse(r#"ab( cde )"#),
            Ok((
                Expr::Call(
                    Uni::Id(Id(String::from("ab"))),
                    vec![Box::new(Expr::Unary(Uni::Id(Id(String::from("cde")))))]
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"ab( cde , fgh )"#),
            Ok((
                Expr::Call(
                    Uni::Id(Id(String::from("ab"))),
                    vec![
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("cde"))))),
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("fgh")))))
                    ]
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
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    )),
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
            expr().easy_parse(r#"1 * 2 * 3 - 5"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Mul,
                        Box::new(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Number(2))),
                            BinOpKind::Mul,
                            Box::new(Expr::Unary(Uni::Number(3))),
                        ))
                    )),
                    BinOpKind::Sub,
                    Box::new(Expr::Unary(Uni::Number(5))),
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"1 * 2 - 3 / 5"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(2))),
                    )),
                    BinOpKind::Sub,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(3))),
                        BinOpKind::Div,
                        Box::new(Expr::Unary(Uni::Number(5))),
                    ))
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"1 + 2 * 3 - 5"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Number(2))),
                            BinOpKind::Mul,
                            Box::new(Expr::Unary(Uni::Number(3))),
                        )),
                        BinOpKind::Sub,
                        Box::new(Expr::Unary(Uni::Number(5))),
                    )),
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
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(2))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(3))),
                    )),
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
