use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{attempt, choice, many, parser, sep_by1, token, Parser};

use crate::expr::uni::*;
use crate::expr::*;

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetExpr(Id, Expr),
    Fn(Id, Args, Vec<Box<Statement>>),
}

#[derive(Debug, PartialEq)]
pub struct Args(Vec<Expr>);

fn let_<I>() -> impl Parser<Input = I, Output = Statement>
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

fn args<I>() -> impl Parser<Input = I, Output = Args>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    token('(')
        .skip(spaces())
        .and(
            attempt(sep_by1(expr(), token(',')).map(|exps| Args(exps)))
                .or(expr().map(|exp| Args(vec![exp]))),
        )
        .skip(spaces())
        .and(token(')'))
        .map(|((_, exps), _)| exps)
}

fn fn_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("fn")
        .skip(spaces())
        .and(expr_())
        .skip(spaces())
        .and(args())
        .skip(spaces())
        .skip(token('{'))
        .skip(spaces())
        .and(many(statement()))
        .skip(spaces())
        .and(token('}'))
        .skip(spaces())
        .map(
            |((((_, id), args), stetements_), _): ((((_, Expr), Args), Vec<Statement>), _)| match id
            {
                Expr::Unary(unary_) => {
                    if let Uni::Id(id_) = unary_ {
                        return Statement::Fn(
                            id_,
                            args,
                            stetements_.into_iter().map(|s| Box::new(s)).collect(),
                        );
                    };
                    panic!("should come Uni::Id. actual: {:?}", unary_);
                }
                _ => panic!("should come Id. actual: {:?}", id),
            },
        )
}

parser! {
    pub fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        choice((let_(), fn_()))
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

    #[test]
    fn fn_test() {
        let input = r#"fn def(a,b) {
          let abc = "aaa";
        }"#;
        assert_eq!(
            statement().easy_parse(input),
            Ok((
                Statement::Fn(
                    Id(String::from("def")),
                    Args(vec![
                        Expr::Unary(Uni::Id(Id(String::from("a")))),
                        Expr::Unary(Uni::Id(Id(String::from("b"))))
                    ]),
                    vec![Box::new(Statement::LetExpr(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa")))
                    ))]
                ),
                ""
            ))
        );
    }
}
