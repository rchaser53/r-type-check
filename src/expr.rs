use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, Parser};

mod binop;
use binop::BinOpKind;

#[derive(Debug, PartialEq)]
pub struct Id(pub String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Id(Id),
    Array(Vec<Expr>),
    String(String),
    Number(i32),
    Boolean(Boolean),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Boolean {
    True,
    False,
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    choice((word(), array(), string(), integer())).skip(skip_spaces())
}

pub fn word<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(letter()).map(|e: String| match e.as_ref() {
        "true" => Expr::Boolean(Boolean::True),
        "false" => Expr::Boolean(Boolean::False),
        _ => Expr::Id(Id(e.into())),
    })
}

pub fn array<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());
    let comma_list = sep_by(expr(), lex_char(','));
    between(lex_char('['), lex_char(']'), comma_list).map(Expr::Array)
}

pub fn string<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());
    between(lex_char('"'), lex_char('"'), many1(letter())).map(Expr::String)
}

pub fn integer<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(digit())
        .map(|string: String| string.parse::<i32>().unwrap())
        .map(Expr::Number)
}

parser! {
    pub fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}

mod test {
    use crate::expr::*;

    #[test]
    fn expr_test() {
        assert_eq!(
            expr().parse(r#"true"#),
            Ok((Expr::Boolean(Boolean::True), ""))
        );
        assert_eq!(expr().parse(r#"123"#), Ok((Expr::Number(123), "")));
    }
}
