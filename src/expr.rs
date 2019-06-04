use combine::error::ParseError;
use combine::parser::choice::optional;
use combine::stream::Stream;
use combine::{attempt, between, many, parser, sep_by, Parser};

use crate::statement::*;
use crate::utils::{skip_spaces, string_skip_spaces, token_skip_spaces};

pub mod bin_op;
use bin_op::{bin_op as bin_op_, BinOpKind};

pub mod uni;
use uni::{field, uni as create_uni, word, word_, Id, Uni};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(Uni),
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
    Call(Vec<Id>, Vec<Box<Expr>>),
    Fn(Id, Args, Vec<Box<Statement>>),
}

#[derive(Debug, PartialEq)]
pub struct Args(Vec<(Id, Id)>);

pub fn create_none_type_id() -> Id {
    Id(String::from("None"))
}

fn args<I>() -> impl Parser<Input = I, Output = Args>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(
        token_skip_spaces('(')
            .with(
                skip_spaces(sep_by(
                    word().and(optional(string_skip_spaces(":").with(skip_spaces(word())))),
                    token_skip_spaces(','),
                ))
                .map(|unis: Vec<(Uni, Option<Uni>)>| {
                    let args = unis
                        .into_iter()
                        .map(|(uni, word)| match word {
                            Some(word_uni) => (uni.id(), word_uni.id()),
                            None => (uni.id(), create_none_type_id()),
                        })
                        .collect();
                    Args(args)
                }),
            )
            .skip(token_skip_spaces(')')),
    )
    .or(token_skip_spaces('(')
        .and(token_skip_spaces(')'))
        .map(|_| Args(vec![])))
}

fn fn_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string_skip_spaces("fn")
        .with(skip_spaces(word()))
        .and(skip_spaces(args()))
        .and(between(
            token_skip_spaces('{'),
            token_skip_spaces('}'),
            many(statement()),
        ))
        .map(
            |((unary_, args), stetements_): ((Uni, Args), Vec<Statement>)| {
                Expr::Fn(
                    unary_.id(),
                    args,
                    stetements_.into_iter().map(|s| Box::new(s)).collect(),
                )
            },
        )
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(fn_())
        .or(handle_op(try_paren_with_binary()))
        .or(try_binary())
}

pub fn unary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(call()).or(create_uni().map(Expr::Unary))
}

pub fn try_paren_with_binary<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token_skip_spaces('('), token_skip_spaces(')'), try_binary())
}

pub fn handle_op<I>(
    input: impl Parser<Input = I, Output = Expr>,
) -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(input)
        .and(skip_spaces(
            attempt(many(skip_spaces(bin_op_()).and(unary())))
                .or(many(skip_spaces(bin_op_()).and(expr()))),
        ))
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
    skip_spaces(attempt(field()).or(word_()))
        .and(between(
            token_skip_spaces('('),
            token_skip_spaces(')'),
            skip_spaces(sep_by(skip_spaces(expr()), token_skip_spaces(',')))
                .map(|exps: Vec<Expr>| exps.into_iter().map(|exp| Box::new(exp)).collect()),
        ))
        .map(|(fn_name, args)| match fn_name {
            Uni::Id(id) => Expr::Call(vec![id], args),
            Uni::Field(fields) => Expr::Call(fields, args),
            _ => panic!("should come Uni::Id. actual: {:?}", fn_name),
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
    fn call_test() {
        assert_eq!(
            expr().easy_parse(r#"ab()"#),
            Ok((Expr::Call(vec![Id(String::from("ab"))], vec![]), ""))
        );

        assert_eq!(
            expr().easy_parse(r#"ab( cde )"#),
            Ok((
                Expr::Call(
                    vec![Id(String::from("ab"))],
                    vec![Box::new(Expr::Unary(Uni::Id(Id(String::from("cde")))))]
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"ab( cde , fgh )"#),
            Ok((
                Expr::Call(
                    vec![Id(String::from("ab"))],
                    vec![
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("cde"))))),
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("fgh")))))
                    ]
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"ab.field( cde , fgh )"#),
            Ok((
                Expr::Call(
                    vec![Id(String::from("ab")), Id(String::from("field"))],
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
    fn call_binary() {
        assert_eq!(
            expr().easy_parse(r#"abc() * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Call(vec![Id(String::from("abc"))], vec![])),
                    BinOpKind::Mul,
                    Box::new(Expr::Unary(Uni::Number(3))),
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"abc( def ) * 3"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Call(
                        vec![Id(String::from("abc"))],
                        vec![Box::new(Expr::Unary(Uni::Id(Id(String::from("def"))))),]
                    )),
                    BinOpKind::Mul,
                    Box::new(Expr::Unary(Uni::Number(3))),
                ),
                ""
            ))
        );

        assert_eq!(
            expr().easy_parse(r#"(abc( def ) + 1) * 2"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Call(
                            vec![Id(String::from("abc"))],
                            vec![Box::new(Expr::Unary(Uni::Id(Id(String::from("def"))))),]
                        )),
                        BinOpKind::Add,
                        Box::new(Expr::Unary(Uni::Number(1))),
                    )),
                    BinOpKind::Mul,
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

        assert_eq!(
            expr().easy_parse(r#"(1 + 2) * (3 * 4)"#),
            Ok((
                Expr::Binary(
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(1))),
                        BinOpKind::Add,
                        Box::new(Expr::Unary(Uni::Number(2))),
                    )),
                    BinOpKind::Mul,
                    Box::new(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Number(3))),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(4))),
                    )),
                ),
                ""
            ))
        );
    }

    #[test]
    fn fn_test() {
        let input = r#"fn def() {
          let abc = "aaa" in
        }"#;
        assert_eq!(
            expr().easy_parse(input),
            Ok((
                Expr::Fn(
                    Id(String::from("def")),
                    Args(vec![]),
                    vec![Box::new(Statement::Let(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa"))),
                        vec![],
                    ))]
                ),
                ""
            ))
        );

        let input = r#"fn def( a ) {
          let abc = "aaa" in
        }"#;
        assert_eq!(
            expr().easy_parse(input),
            Ok((
                Expr::Fn(
                    Id(String::from("def")),
                    Args(vec![(Id(String::from("a")), create_none_type_id())]),
                    vec![Box::new(Statement::Let(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa"))),
                        vec![],
                    ))]
                ),
                ""
            ))
        );

        let input = r#"fn def( a, b ) {
          let abc = "aaa" in
        }"#;
        assert_eq!(
            expr().easy_parse(input),
            Ok((
                Expr::Fn(
                    Id(String::from("def")),
                    Args(vec![
                        (Id(String::from("a")), create_none_type_id()),
                        (Id(String::from("b")), create_none_type_id())
                    ]),
                    vec![Box::new(Statement::Let(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa"))),
                        vec![],
                    ))]
                ),
                ""
            ))
        );
    }
}
