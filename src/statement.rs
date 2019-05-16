use combine::error::ParseError;
use combine::parser::char::{spaces, string};
use combine::stream::Stream;
use combine::{attempt, choice, many, parser, sep_by, token, Parser};

use crate::expr::bin_op::BinOpKind;
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
        .and(word())
        .skip(spaces())
        .skip(token('='))
        .skip(spaces())
        .and(expr_())
        .skip(spaces())
        .and(token(';'))
        .skip(spaces())
        .map(|(((_, unary_), value), _)| {
            if let Uni::Id(id_) = unary_ {
                return Statement::LetExpr(id_, value);
            };
            panic!("should come Uni::Id. actual: {:?}", unary_);
        })
}

fn args<I>() -> impl Parser<Input = I, Output = Args>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(
        token('(')
            .skip(spaces())
            .and(sep_by(unary(), token(',').skip(spaces())).map(|exps| Args(exps)))
            .skip(spaces())
            .and(token(')'))
            .map(|((_, exps), _)| exps),
    )
    .or(token('(')
        .skip(spaces())
        .and(token(')'))
        .map(|_| Args(vec![])))
}

fn fn_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("fn")
        .skip(spaces())
        .and(unary())
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
        assert_eq!(
            statement().easy_parse(r#"let abc = "aaa";"#),
            Ok((
                Statement::LetExpr(
                    Id(String::from("abc")),
                    Expr::Unary(Uni::String(String::from("aaa")))
                ),
                ""
            ))
        );

        assert_eq!(
            statement().easy_parse(r#"let abc = (1 + 3) * 4;"#),
            Ok((
                Statement::LetExpr(
                    Id(String::from("abc")),
                    Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Number(1))),
                            BinOpKind::Add,
                            Box::new(Expr::Unary(Uni::Number(3))),
                        )),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(4))),
                    ),
                ),
                ""
            ))
        );
    }

    #[test]
    fn fn_test() {
        let input = r#"fn def() {
          let abc = "aaa";
        }"#;
        assert_eq!(
            statement().easy_parse(input),
            Ok((
                Statement::Fn(
                    Id(String::from("def")),
                    Args(vec![]),
                    vec![Box::new(Statement::LetExpr(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa")))
                    ))]
                ),
                ""
            ))
        );

        let input = r#"fn def( a ) {
          let abc = "aaa";
        }"#;
        assert_eq!(
            statement().easy_parse(input),
            Ok((
                Statement::Fn(
                    Id(String::from("def")),
                    Args(vec![Expr::Unary(Uni::Id(Id(String::from("a")))),]),
                    vec![Box::new(Statement::LetExpr(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa")))
                    ))]
                ),
                ""
            ))
        );

        let input = r#"fn def( a, b ) {
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
