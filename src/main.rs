#[macro_use]
extern crate combine;
use combine::error::{ParseError, ParseResult};
use combine::parser::char::{char, letter, spaces};
use combine::stream::state::State;
use combine::stream::{Positioned, Stream};
use combine::{between, choice, many1, parser, sep_by, Parser};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Id(String),
    Array(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
}

pub enum Statement {
    Expr(Expr, Expr)
}

fn expr_<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let word = many1(letter());
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());

    let comma_list = sep_by(expr(), lex_char(','));
    let array = between(lex_char('['), lex_char(']'), comma_list);

    let pair = (lex_char('('), expr(), lex_char(';'), expr(), lex_char(')'))
        .map(|t| Expr::Pair(Box::new(t.1), Box::new(t.3)));

    choice((word.map(Expr::Id), array.map(Expr::Array), pair)).skip(skip_spaces())
}

parser! {
    fn expr[I]()(I) -> Expr
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}

fn main() {
    let result = expr().parse("[]; (hello, world); [rust];");
    dbg!(result);
}
