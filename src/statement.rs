use combine::error::{ParseError, ParseResult};
use combine::parser::char::{char, letter, spaces, string};
use combine::stream::state::State;
use combine::stream::Stream;
use combine::{between, choice, many1, parser, sep_by, token, Parser};

use crate::expr::*;

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetExpr(Id, Expr),
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
        .skip(spaces())
        .and(token(';'))
        .skip(spaces())
        .map(|(((_, id), value), _)| match id {
            Expr::Id(id) => Statement::LetExpr(id, value),
            _ => panic!("should come Id. actual: {:?}", id),
        })
}

parser! {
    fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        statement_()
    }
}

mod test {
    use super::expr::*;
    use super::statement::*;

    #[test]
    fn let_fn() {
        let input = r#"let abc = "aaa";"#;
        assert_eq!(
            statement().parse(input),
            Ok((
                Statement::LetExpr(Id(String::from("abc")), Expr::String(String::from("aaa"))),
                ""
            ))
        );
    }
}