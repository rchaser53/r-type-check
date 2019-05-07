use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{parser, token, Parser};

use crate::expr::uni::*;
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
            Expr::Unary(unary_) => {
                if let Uni::Id(id_) = unary_ {
                    return Statement::LetExpr(id_, value);
                };
                panic!("should come Uni::Id. actual: {:?}", unary_);
            }
            _ => panic!("should come Id. actual: {:?}", id),
        })
}

parser! {
    pub fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        statement_()
    }
}

mod test {
    use crate::statement::*;

    #[test]
    fn let_test() {
        let input = r#"let abc = "aaa";"#;
        assert_eq!(
            statement().easy_parse(input),
            Ok((
                Statement::LetExpr(
                    Id(String::from("abc")),
                    Expr::Unary(Uni::String(String::from("aaa")))
                ),
                ""
            ))
        );
    }
}
