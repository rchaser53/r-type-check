use combine::parser::choice::optional;
use combine::{attempt, between, choice, many, parser};

use crate::new_expr::uni::*;
use crate::new_expr::Node::*;
use crate::new_expr::*;
use crate::new_utils::{skip_spaces, string_skip_spaces, token_skip_spaces};
use crate::pos::MyStream;

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
   pub fn statement['a]()(MyStream<'a>) -> Statement
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

parser! {
   pub fn if_condition['a]()(MyStream<'a>) -> Expr
    {
        between(token_skip_spaces('('), token_skip_spaces(')'), expr()).map(|expr| expr)
    }
}

parser! {
   pub fn assign_['a]()(MyStream<'a>) -> Assign
    {
        skip_spaces(word())
            .and(token_skip_spaces('='))
            .and(skip_spaces(expr_()))
            .skip(token_skip_spaces(';'))
            .map(|((unary_, _), mut value)| {
                if let Uni::Id(id_) = unary_ {
                    value.renew_parent_id(id_.clone());
                    return Assign(id_, value);
                };
                unreachable!()
            })
    }
}

parser! {
   pub fn return_['a]()(MyStream<'a>) -> Statement
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
}

parser! {
   pub fn let_['a]()(MyStream<'a>) -> Statement
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
}

parser! {
   pub fn expr_statement['a]()(MyStream<'a>) -> Statement
    {
        attempt(expr_statement_with_semicolon()).or(expr_statement_no_semicolon())
    }
}

parser! {
   pub fn expr_statement_with_semicolon['a]()(MyStream<'a>) -> Statement
    {
        skip_spaces(expr())
            .skip(token_skip_spaces(';'))
            .map(|exp| Statement::Expr(exp))
    }
}

parser! {
   pub fn expr_statement_no_semicolon['a]()(MyStream<'a>) -> Statement
    {
        skip_spaces(expr()).map(|exp| Statement::Expr(exp))
    }
}

parser! {
   pub fn if_['a]()(MyStream<'a>) -> Statement
    {
        let create_if = || {
            skip_spaces(if_condition())
                .and(between(
                    token_skip_spaces('{'),
                    token_skip_spaces('}'),
                    many(statement()),
                ))
                .map(|(cond, stetements_): (Expr, Vec<Statement>)| {
                    (cond, stetements_.into_iter().map(|s| Box::new(s)).collect())
                })
        };

        let else_if = || { many(attempt(
            string_skip_spaces("else").with(string_skip_spaces("if").with(create_if())),
        ))
        .and(optional(string_skip_spaces("else").with(between(
            token_skip_spaces('{'),
            token_skip_spaces('}'),
            many(statement()),
        ))))
        .map(
            |(mut else_ifs, else_statement): (
                Vec<(Expr, Vec<Box<Statement>>)>,
                Option<Vec<Statement>>,
            )| {
                if let Some(else_statement) = else_statement {
                    else_ifs.push((
                        Expr::new(Unary(Uni::Boolean(Boolean::True))),
                        else_statement.into_iter().map(|s| Box::new(s)).collect(),
                    ));
                }
                else_ifs
            },
        )
        };

        string_skip_spaces("if")
          .with(create_if())
          .and(else_if()).map(
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
}

parser! {
   pub fn assign['a]()(MyStream<'a>) -> Statement
    {
        assign_().map(|assign| Statement::Assign(assign))
    }
}

mod test {
    use combine::stream::state::{State, SourcePosition};
    use combine::Parser;
    use combine::easy;

    use crate::new_expr::bin_op::*;
    use crate::new_statement::*;

    fn helper<'a>(input: State<&'a str, SourcePosition>) -> Result<Statement, easy::Errors<char, &'a str, SourcePosition>> {
        statement()
            .easy_parse(input)
            .map(|r| r.0)
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
            State::new(r#"abc = "aaa";"#),
            Statement::Assign(Assign(
                Id(String::from("abc")),
                Expr::new(Unary(Uni::String(String::from("aaa"))))
            ))
        );
    }

    #[test]
    fn return_test() {
        assert_statement!(
            State::new(r#"return "aaa";"#),
            Statement::Return(Expr::new(Unary(Uni::String(String::from("aaa")))))
        );
    }

    #[test]
    fn if_test() {
        assert_statement!(
            State::new(r#"if (i < 10) {
              let abc = "aaa" in
            }"#),
            Statement::If(vec![(
                Expr::new(Binary(
                    Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                    BinOpKind::Lt,
                    Box::new(Expr::new(Unary(Uni::Number(10))))
                )),
                vec![Box::new(Statement::Let(
                    vec![Assign(
                        Id(String::from("abc")),
                        Expr::new(Unary(Uni::String(String::from("aaa")))),
                    )],
                    vec![],
                ))]
            )])
        );

        assert_statement!(
            State::new(r#"if (i < 10) {
              let abc = "aaa" in
            } else {
              let def = "bbb" in
            }"#),
            Statement::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("abc")),
                            Expr::new(Unary(Uni::String(String::from("aaa")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Unary(Uni::Boolean(Boolean::True))),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("def")),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                )
            ])
        );

        assert_statement!(
            State::new(r#"if (i < 10) {
              let abc = "aaa" in
            } else if (j > 100) {
              let def = "bbb" in
            }"#),
            Statement::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("abc")),
                            Expr::new(Unary(Uni::String(String::from("aaa")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("j")))))),
                        BinOpKind::Gt,
                        Box::new(Expr::new(Unary(Uni::Number(100))))
                    )),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("def")),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                )
            ])
        );

        assert_statement!(
            State::new(r#"if (i < 10) {
              let abc = "aaa" in
            } else if (j > 100) {
              let def = "bbb" in
            } else {
              let ghi = "ccc" in
            }"#),
            Statement::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("abc")),
                            Expr::new(Unary(Uni::String(String::from("aaa")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("j")))))),
                        BinOpKind::Gt,
                        Box::new(Expr::new(Unary(Uni::Number(100))))
                    )),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("def")),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Unary(Uni::Boolean(Boolean::True))),
                    vec![Box::new(Statement::Let(
                        vec![Assign(
                            Id(String::from("ghi")),
                            Expr::new(Unary(Uni::String(String::from("ccc")))),
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
            State::new(r#"1 + 2;"#),
            Statement::Expr(Expr::new(Binary(
                Box::new(Expr::new(Unary(Uni::Number(1)))),
                BinOpKind::Add,
                Box::new(Expr::new(Unary(Uni::Number(2)))),
            )))
        );
    }

    #[test]
    fn let_test() {
        assert_statement!(
            State::new(r#"let abc = "aaa" in
              abc + "def";
            "#),
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::new(Unary(Uni::String(String::from("aaa")))),
                )],
                vec![Box::new(Statement::Expr(Expr::new(Binary(
                    Box::new(Expr::new(Unary(Uni::Id(Id(String::from("abc")))))),
                    BinOpKind::Add,
                    Box::new(Expr::new(Unary(Uni::String(String::from("def"))))),
                )))),]
            )
        );

        assert_statement!(
            State::new(r#"let abc = fn(aaa) {
              return aaa + 123;
            } in
              abc(456) + "def";
            "#),
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::new(Fn(Function(
                        vec![Id(String::from("aaa"))],
                        vec![Box::new(Statement::Return(Expr::new(Binary(
                            Box::new(Expr::new(Unary(Uni::Id(Id(String::from("aaa")))))),
                            BinOpKind::Add,
                            Box::new(Expr::new(Unary(Uni::Number(123)))),
                        ))))]
                    ))),
                )],
                vec![Box::new(Statement::Expr(Expr::new(Binary(
                    Box::new(Expr::new(Call(
                        Field::new(None, Id(String::from("abc")), None),
                        vec![Box::new(Expr::new(Unary(Uni::Number(456))))]
                    ))),
                    BinOpKind::Add,
                    Box::new(Expr::new(Unary(Uni::String(String::from("def"))))),
                )))),]
            )
        );

        assert_statement!(
            State::new(r#"let abc = (1 + 3) * 4 in (
              abc = abc + 2;
              return abc;
            )"#),
            Statement::Let(
                vec![Assign(
                    Id(String::from("abc")),
                    Expr::new(Binary(
                        Box::new(Expr::new(Binary(
                            Box::new(Expr::new(Unary(Uni::Number(1)))),
                            BinOpKind::Add,
                            Box::new(Expr::new(Unary(Uni::Number(3)))),
                        ))),
                        BinOpKind::Mul,
                        Box::new(Expr::new(Unary(Uni::Number(4)))),
                    ))
                )],
                vec![
                    Box::new(Statement::Assign(Assign(
                        Id(String::from("abc")),
                        Expr::new(Binary(
                            Box::new(Expr::new(Unary(Uni::Id(Id(String::from("abc")))))),
                            BinOpKind::Add,
                            Box::new(Expr::new(Unary(Uni::Number(2)))),
                        ))
                    )),),
                    Box::new(Statement::Return(Expr::new(Unary(Uni::Id(Id(
                        String::from("abc")
                    ))))))
                ],
            )
        );
    }
}
