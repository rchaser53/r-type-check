use combine::error::ParseError;
use combine::parser::choice::optional;
use combine::stream::Stream;
use combine::{attempt, between, choice, easy, many, parser, Parser};

use crate::expr::uni::*;
use crate::expr::*;
use crate::utils::{skip_spaces, string_skip_spaces, token_skip_spaces};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(Vec<Assign>, Vec<Box<Statement>>),
    Expr(Expr),
    Assign(Assign),
    If(Vec<(Expr, Vec<Box<Statement>>)>),
    Return(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assign(pub Id, pub Expr);

#[derive(Clone, Debug, PartialEq)]
pub struct ForCondition(Box<Statement>, Box<Statement>, Box<Statement>);

parser! {
    pub fn statement[I]()(I) -> Statement
    where [I: Stream<Item = char>]
    {
        choice(
            (
                attempt(return_()),
                attempt(if_()),
                attempt(let_()),
                attempt(assign()),
                attempt(expr_statement())
            )
        )
    }
}

fn if_condition<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token_skip_spaces('('), token_skip_spaces(')'), expr()).map(|expr| expr)
}

fn assign_<I>() -> impl Parser<Input = I, Output = Assign>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(word())
        .and(token_skip_spaces('='))
        .and(skip_spaces(expr_()))
        .skip(token_skip_spaces(';'))
        .map(|((unary_, _), value)| {
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
        .with(skip_spaces(expr_statement()))
        .map(|value| {
            if let Statement::Expr(exp) = value {
                Statement::Return(exp)
            } else {
                unreachable!()
            }
        })
}

fn let_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        string_skip_spaces("let"),
        string_skip_spaces("in"),
        many(attempt(
            skip_spaces(skip_spaces(word()).and(token_skip_spaces('=').with(skip_spaces(expr_()))))
                .map(|(uni, exp)| Assign(uni.id(), exp)),
        )),
    )
    .and(
        attempt(between(
            token_skip_spaces('('),
            token_skip_spaces(')'),
            many(statement()),
        ))
        .or(many(statement())),
    )
    .map(|(lets, statements): (Vec<Assign>, Vec<Statement>)| {
        Statement::Let(lets, statements.into_iter().map(|s| Box::new(s)).collect())
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
        .skip(token_skip_spaces(';'))
        .map(|exp| Statement::Expr(exp))
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
    create_if(string_skip_spaces("if")).and(else_if_()).map(
        |((cond, stetements_), mut elses): (
            (Expr, Vec<Box<Statement>>),
            Vec<(Expr, Vec<Box<Statement>>)>,
        )| {
            let mut if_vecs = vec![(cond, stetements_)];
            if_vecs.append(&mut elses);
            Statement::If(if_vecs)
        },
    )
}

type IfCombination = (Expr, Vec<Box<Statement>>);
fn if_else_<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    create_if(string_skip_spaces("if"))
        .and(optional(
            string_skip_spaces("else")
                .with(optional(
                    string_skip_spaces("if").with(skip_spaces(if_condition())),
                ))
                .and(between(
                    token_skip_spaces('{'),
                    token_skip_spaces('}'),
                    many(statement()),
                ))
                .map(
                    |(else_condition, else_statements): (Option<Expr>, Vec<Statement>)| {
                        let else_condition = if let Some(cond) = else_condition {
                            cond
                        } else {
                            Expr::Unary(Uni::Boolean(Boolean::True))
                        };
                        (
                            else_condition,
                            else_statements.into_iter().map(|s| Box::new(s)).collect(),
                        )
                    },
                ),
        ))
        .map(
            |((if_condition, if_statements), else_combination): (
                IfCombination,
                Option<IfCombination>,
            )| {
                if let Some((else_condition, else_statements)) = else_combination {
                    Statement::If(vec![
                        (if_condition, if_statements),
                        (else_condition, else_statements),
                    ])
                } else {
                    Statement::If(vec![(if_condition, if_statements)])
                }
            },
        )
}

fn else_if_<I>() -> impl Parser<Input = I, Output = Vec<(Expr, Vec<Box<Statement>>)>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(
        many(create_if(
            string_skip_spaces("else").with(string_skip_spaces("if")),
        ))
        .and(string_skip_spaces("else").with(between(
            token_skip_spaces('{'),
            token_skip_spaces('}'),
            many(statement()),
        )))
        .map(
            |(mut else_ifs, statements_): (Vec<(Expr, Vec<Box<Statement>>)>, Vec<Statement>)| {
                else_ifs.push((
                    Expr::Unary(Uni::Boolean(Boolean::True)),
                    statements_.into_iter().map(|s| Box::new(s)).collect(),
                ));
                else_ifs
            },
        ),
    )
    .or(attempt(many(create_if(
        string_skip_spaces("else").with(string_skip_spaces("if")),
    )))
    .or(string_skip_spaces("else")
        .with(between(
            token_skip_spaces('{'),
            token_skip_spaces('}'),
            many(statement()),
        ))
        .map(|statements_: Vec<Statement>| {
            vec![(
                Expr::Unary(Uni::Boolean(Boolean::True)),
                statements_.into_iter().map(|s| Box::new(s)).collect(),
            )]
        })))
}

fn create_if<I, T>(
    input: impl Parser<Input = I, Output = T>,
) -> impl Parser<Input = I, Output = (Expr, Vec<Box<Statement>>)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    input
        .with(skip_spaces(if_condition()))
        .and(between(
            token_skip_spaces('{'),
            token_skip_spaces('}'),
            many(statement()),
        ))
        .map(|(cond, stetements_): (Expr, Vec<Statement>)| {
            (cond, stetements_.into_iter().map(|s| Box::new(s)).collect())
        })
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

    fn helper<'a>(input: &'a str) -> Result<Statement, easy::Errors<char, &'a str, usize>> {
        statement()
            .easy_parse(input)
            .map(|r| r.0)
            .map_err(|err| err.map_position(|p| p.translate_position(input)))
    }

    macro_rules! assert_statement {
        ($input: expr, $expected: expr) => {
            let statement = match helper($input) {
                Ok(statement) => statement,
                Err(err) => panic!(err),
            };
            assert_eq!(statement, $expected);
        };
    }

    #[test]
    fn assign_test() {
        assert_statement!(
            r#"abc = "aaa";"#,
            Statement::Assign(Assign(
                Id(String::from("abc")),
                Expr::Unary(Uni::String(String::from("aaa")))
            ))
        );
    }

    #[test]
    fn return_test() {
        assert_statement!(
            r#"return "aaa";"#,
            Statement::Return(Expr::Unary(Uni::String(String::from("aaa"))))
        );
    }

    #[test]
    fn if_test() {
        assert_statement!(
            r#"if (i < 10) {
              let abc = "aaa" in
            }"#,
            Statement::If(vec![(
                Expr::Binary(
                    Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                    BinOpKind::Lt,
                    Box::new(Expr::Unary(Uni::Number(10)))
                ),
                vec![Box::new(Statement::Let(
                    vec![Assign(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::String(String::from("aaa"))),
                    )],
                    vec![],
                ))]
            )])
        );

        assert_statement!(
            r#"if (i < 10) {
              let abc = "aaa" in
            } else {
              let def = "bbb" in
            }"#,
            Statement::If(vec![
                (
                    Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                        BinOpKind::Lt,
                        Box::new(Expr::Unary(Uni::Number(10)))
                    ),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("abc")),
                            Expr::Unary(Uni::String(String::from("aaa"))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::Unary(Uni::Boolean(Boolean::True)),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("def")),
                            Expr::Unary(Uni::String(String::from("bbb"))),
                        )],
                        vec![],
                    ))]
                )
            ])
        );

        assert_statement!(
            r#"if (i < 10) {
              let abc = "aaa" in
            } else if (j > 100) {
              let def = "bbb" in
            }"#,
            Statement::If(vec![
                (
                    Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("i"))))),
                        BinOpKind::Lt,
                        Box::new(Expr::Unary(Uni::Number(10)))
                    ),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("abc")),
                            Expr::Unary(Uni::String(String::from("aaa"))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::Binary(
                        Box::new(Expr::Unary(Uni::Id(Id(String::from("j"))))),
                        BinOpKind::Gt,
                        Box::new(Expr::Unary(Uni::Number(100)))
                    ),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("def")),
                            Expr::Unary(Uni::String(String::from("bbb"))),
                        )],
                        vec![],
                    ))]
                )
            ])
        );
    }

    #[test]
    fn expr_statement_test() {
        assert_statement!(
            r#"1 + 2;"#,
            Statement::Expr(Expr::Binary(
                Box::new(Expr::Unary(Uni::Number(1))),
                BinOpKind::Add,
                Box::new(Expr::Unary(Uni::Number(2))),
            ))
        );
    }

    #[test]
    fn let_test() {
        assert_statement!(
            r#"let abc = "aaa" in
              abc + "def";
            "#,
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::Unary(Uni::String(String::from("aaa"))),
                )],
                vec![Box::new(Statement::Expr(Expr::Binary(
                    Box::new(Expr::Unary(Uni::Id(Id(String::from("abc"))))),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::String(String::from("def")))),
                ))),]
            )
        );

        assert_statement!(
            r#"let abc = fn(aaa) {
              return aaa + 123;
            } in
              abc(456) + "def";
            "#,
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::Fn(
                        vec![Id(String::from("aaa"))],
                        vec![Box::new(Statement::Return(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Id(Id(String::from("aaa"))))),
                            BinOpKind::Add,
                            Box::new(Expr::Unary(Uni::Number(123))),
                        )))]
                    ),
                )],
                vec![Box::new(Statement::Expr(Expr::Binary(
                    Box::new(Expr::Call(
                        vec![Id(String::from("abc"))],
                        vec![Box::new(Expr::Unary(Uni::Number(456)))]
                    )),
                    BinOpKind::Add,
                    Box::new(Expr::Unary(Uni::String(String::from("def")))),
                ))),]
            )
        );

        assert_statement!(
            r#"let abc = (1 + 3) * 4 in (
              abc = abc + 2;
              return abc;
            )"#,
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::Binary(
                        Box::new(Expr::Binary(
                            Box::new(Expr::Unary(Uni::Number(1))),
                            BinOpKind::Add,
                            Box::new(Expr::Unary(Uni::Number(3))),
                        )),
                        BinOpKind::Mul,
                        Box::new(Expr::Unary(Uni::Number(4))),
                    )
                )],
                vec![
                    Box::new(Statement::Assign(Assign(
                        Id(String::from("abc")),
                        Expr::Binary(
                            Box::new(Expr::Unary(Uni::Id(Id(String::from("abc"))))),
                            BinOpKind::Add,
                            Box::new(Expr::Unary(Uni::Number(2))),
                        )
                    )),),
                    Box::new(Statement::Return(Expr::Unary(Uni::Id(Id(String::from(
                        "abc"
                    ))))))
                ],
            )
        );
    }
}
