use combine::error::{ParseError, ParseResult};
use combine::parser::char::{char, letter, spaces, string};
use combine::stream::state::State;
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, token, Parser};

#[derive(Debug, PartialEq)]
pub struct Id(String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Id(Id),
    Array(Vec<Expr>),
    String(String),
    Pair(Box<Expr>, Box<Expr>),
}

pub fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let word = many1(letter()).map(|e| Expr::Id(Id(e)));
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());

    let string = between(lex_char('"'), lex_char('"'), many1(letter()));

    let comma_list = sep_by(expr(), lex_char(','));
    let array = between(lex_char('['), lex_char(']'), comma_list);

    let pair = (lex_char('('), expr(), lex_char(';'), expr(), lex_char(')'))
        .map(|t| Expr::Pair(Box::new(t.1), Box::new(t.3)));

    choice((word, string.map(Expr::String), array.map(Expr::Array), pair)).skip(skip_spaces())
}

parser! {
    pub fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}
