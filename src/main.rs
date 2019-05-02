#[macro_use]
extern crate combine;
use combine::error::{ParseError, ParseResult};
use combine::parser::char::{char, letter, spaces, string};
use combine::stream::state::State;
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, token, Parser};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Id(String),
    Array(Vec<Expr>),
    Pair(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expr(Expr, Expr),
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

fn statement_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("let")
        .skip(spaces())
        .and(expr_())
        .skip(spaces())
        .skip(token('='))
        .skip(spaces())
        .and(expr_())
        .skip(token(';'))
        .map(|((_, id), value)| Statement::Expr(id, value))
}

parser! {
    fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        statement_()
    }
}

fn main() {
    dbg!(expr().parse("[]; (hello, world); [rust];"));

    dbg!(statement().parse("let abc = aaa;"));
}
