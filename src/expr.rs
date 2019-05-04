use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{between, choice, many, many1, one_of, parser, sep_by, Parser};

#[derive(Debug, PartialEq)]
pub struct Id(pub String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Id(Id),
    Array(Vec<Expr>),
    String(String),
    Number(i32),
    Boolean(Boolean),
}

#[derive(Debug, PartialEq)]
enum Boolean {
    True,
    False,
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let word = many1(letter()).map(|e: String| match e.as_ref() {
        "true" => Expr::Boolean(Boolean::True),
        "false" => Expr::Boolean(Boolean::False),
        _ => Expr::Id(Id(e.into())),
    });
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());

    let comma_list = sep_by(expr(), lex_char(','));
    let array = between(lex_char('['), lex_char(']'), comma_list);

    let string = between(lex_char('"'), lex_char('"'), many1(letter()));
    let integer =
        many1(digit()).map(|string: String| string.parse::<i32>().unwrap());

    choice((
        word,
        array.map(Expr::Array),
        string.map(Expr::String),
        integer.map(Expr::Number),
    ))
    .skip(skip_spaces())
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
