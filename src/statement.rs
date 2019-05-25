use combine::error::ParseError;
use combine::stream::Stream;
use combine::{attempt, choice, many, parser, Parser};

use crate::expr::uni::*;
use crate::expr::*;

use crate::utils::{skip_spaces, string_skip_spaces, token_skip_spaces};

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetExpr(Id, Expr),
    Expr(Expr),
    Assign(Assign),
    For(ForCondition, Vec<Box<Statement>>),
    If(Vec<(IfCondition, Vec<Box<Statement>>)>),
    Return(Box<Statement>),
}

#[derive(Debug, PartialEq)]
pub struct Assign(Id, Expr);

#[derive(Debug, PartialEq)]
pub struct ForCondition(Box<Statement>, Box<Statement>, Box<Statement>);

#[derive(Debug, PartialEq)]
pub struct IfCondition(Box<Statement>);

parser! {
    pub fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        choice((for_(), return_(), if_(), let_(), assign(), expr_statement()))
    }
}

fn if_condition<I>() -> impl Parser<Input = I, Output = IfCondition>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    token_skip_spaces('(')
        .and(skip_spaces(expr_statement_no_semicolon()))
        .and(token_skip_spaces(')'))
        .map(|((_, cond), _)| IfCondition(Box::new(cond)))
}

fn for_condition<I>() -> impl Parser<Input = I, Output = ForCondition>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    token_skip_spaces('(')
        .and(skip_spaces(let_()))
        .and(skip_spaces(expr_statement()))
        .and(skip_spaces(expr_statement_no_semicolon()))
        .and(token_skip_spaces(')'))
        .map(|((((_, first), limit), iterate), _)| {
            ForCondition(Box::new(first), Box::new(limit), Box::new(iterate))
        })
}

fn assign_<I>() -> impl Parser<Input = I, Output = Assign>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(word())
        .and(token_skip_spaces('='))
        .and(skip_spaces(expr_()))
        .and(token_skip_spaces(';'))
        .map(|(((unary_, _), value), _)| {
            if let Uni::Id(id_) = unary_ {
                return Assign(id_, value);
            };
            panic!("should come Uni::Id. actual: {:?}", unary_);
        })
}

fn return_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string_skip_spaces("return")
        .and(skip_spaces(expr_statement()))
        .map(|(_, value)| Statement::Return(Box::new(value)))
}

fn let_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string_skip_spaces("let")
        .and(skip_spaces(word()))
        .and(token_skip_spaces('='))
        .and(skip_spaces(expr_()))
        .and(token_skip_spaces(';'))
        .map(|((((_, unary_), _), value), _)| {
            if let Uni::Id(id_) = unary_ {
                return Statement::LetExpr(id_, value);
            };
            panic!("should come Uni::Id. actual: {:?}", unary_);
        })
}

fn expr_statement<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(expr_statement_with_semicolon()).or(expr_statement_no_semicolon())
}

fn expr_statement_with_semicolon<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(expr())
        .and(token_skip_spaces(';'))
        .map(|(exp, _)| Statement::Expr(exp))
}

fn expr_statement_no_semicolon<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(expr()).map(|exp| Statement::Expr(exp))
}

fn if_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    create_if("if").map(|(cond, stetements_)| Statement::If(vec![(cond, stetements_)]))
}

fn create_if<I>(
    input: &'static str,
) -> impl Parser<Input = I, Output = (IfCondition, Vec<Box<Statement>>)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string_skip_spaces(input)
        .and(skip_spaces(if_condition()))
        .and(token_skip_spaces('{'))
        .and(skip_spaces(many(statement())))
        .and(token_skip_spaces('}'))
        .map(
            |((((_, cond), _), stetements_), _): ((((_, IfCondition), _), Vec<Statement>), _)| {
                (cond, stetements_.into_iter().map(|s| Box::new(s)).collect())
            },
        )
}

fn for_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string_skip_spaces("for")
        .and(skip_spaces(for_condition()))
        .and(token_skip_spaces('{'))
        .and(skip_spaces(many(statement())))
        .and(token_skip_spaces('}'))
        .map(
            |((((_, cond), _), stetements_), _): ((((_, ForCondition), _), Vec<Statement>), _)| {
                Statement::For(cond, stetements_.into_iter().map(|s| Box::new(s)).collect())
            },
        )
}

fn assign<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    assign_().map(|assign| Statement::Assign(assign))
}

mod test {
    use crate::expr::bin_op::*;
    use crate::statement::*;

    #[test]
    fn for_condition_test() {
        assert_eq!(
            for_condition().easy_parse(r#"(let i = 0; i < 10; i + 1)"#),
            Ok((
                ForCondition(
                    Box::new(Statement::LetExpr(
                        Id(String::from("i")),
                        Expr::Unary(Uni::Number(0)),
                    )),
                    Box::new(Statement::Expr(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                        BinOpKind::Lt,
                        Box::new(Expr::Unary(Uni::Number(10)))
                    ))),
                    Box::new(Statement::Expr(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                        BinOpKind::Add,
                        Box::new(Expr::Unary(Uni::Number(1)))
                    )))
                ),
                ""
            ))
        );
    }

    #[test]
    fn assign_test() {
        assert_eq!(
            statement().easy_parse(r#"abc = "aaa";"#),
            Ok((
                Statement::Assign(Assign(
                    Id(String::from("abc")),
                    Expr::Unary(Uni::String(String::from("aaa")))
                )),
                ""
            ))
        );
    }

    #[test]
    fn return_test() {
        assert_eq!(
            statement().easy_parse(r#"return "aaa";"#),
            Ok((
                Statement::Return(Box::new(Statement::Expr(Expr::Unary(Uni::String(
                    String::from("aaa")
                ))))),
                ""
            ))
        );
    }

    #[test]
    fn if_test() {
        assert_eq!(
            statement().easy_parse(
                r#"if (i < 10) {
              let abc = "aaa";
            }"#
            ),
            Ok((
                Statement::If(vec![(
                    IfCondition(Box::new(Statement::Expr(Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                        BinOpKind::Lt,
                        Box::new(Expr::Unary(Uni::Number(10)))
                    ))),),
                    vec![Box::new(Statement::LetExpr(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa")))
                    ))]
                )]),
                ""
            ))
        );
    }

    #[test]
    fn for_test() {
        assert_eq!(
            statement().easy_parse(
                r#"for (let i = 0; i < 10; i + 1) {
              let abc = "aaa";
            }"#
            ),
            Ok((
                Statement::For(
                    ForCondition(
                        Box::new(Statement::LetExpr(
                            Id(String::from("i")),
                            Expr::Unary(Uni::Number(0)),
                        )),
                        Box::new(Statement::Expr(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                            BinOpKind::Lt,
                            Box::new(Expr::Unary(Uni::Number(10)))
                        ))),
                        Box::new(Statement::Expr(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                            BinOpKind::Add,
                            Box::new(Expr::Unary(Uni::Number(1)))
                        )))
                    ),
                    vec![Box::new(Statement::LetExpr(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa")))
                    ))]
                ),
                ""
            ))
        );
    }

    #[test]
    fn expr_statement_test() {
        assert_eq!(
            statement().easy_parse(r#"1 + 2;"#),
            Ok((
                Statement::Expr(Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(1))),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::Number(2))),
                )),
                ""
            ))
        );
    }

    #[test]
    fn if_condition_test() {
        assert_eq!(
            if_condition().easy_parse(r#"(10 > x)"#),
            Ok((
                IfCondition(Box::new(Statement::Expr(Expr::Binary(
                    Box::new(Expr::Unary(Uni::Number(10))),
                    BinOpKind::Gt,
                    Box::new(Expr::Unary(Uni::Id(Id(String::from("x")))))
                )))),
                ""
            ))
        );
    }

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
}
