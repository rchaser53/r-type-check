use combine::parser::choice::optional;
use combine::stream::state::SourcePosition;
use combine::{attempt, between, choice, many, parser, position};

use crate::expr::uni::*;
use crate::expr::Node::*;
use crate::expr::*;
use crate::pos::{MyStream, Position};
use crate::utils::*;

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    Let(Vec<Assign>, Vec<Statement>),
    Expr(Expr),
    Assign(Assign),
    If(Vec<(Expr, Vec<Statement>)>),
    Return(Expr),
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub id: Id,
    pub node: StmtKind,
    pub position: Position,
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl Statement {
    pub fn new(node: StmtKind) -> Self {
        Statement {
            id: ID_POOL.next_id(),
            node,
            position: Default::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assign(pub Accessiable, pub Expr);

#[derive(Clone, Debug, PartialEq)]
pub struct ForCondition(Box<Statement>, Box<Statement>, Box<Statement>);

parser! {
   pub fn statement['a]()(MyStream<'a>) -> Statement
    {
        position()
        .and(choice(
            (
                attempt(return_()),
                attempt(if_()),
                attempt(let_()),
                attempt(assign()),
                attempt(expr_statement())
            )
        ))
        .and(position())
        .map(|((lo, mut stmt), hi): ((SourcePosition, Statement), SourcePosition)| {
            stmt.position.hi = hi;
            stmt.position.lo = lo;
            stmt
        })
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
        skip_spaces(attempt(index()).or(field()))
            .and(token_skip_spaces('='))
            .and(skip_spaces(expr()))
            .skip(token_skip_spaces(';'))
            .map(|((unary_, _), mut value)| {
                match unary_ {
                    Uni::Field(field) => {
                        value.renew_parent_id(field.clone());
                        return Assign(Accessiable::Field(field), value);
                    },
                    Uni::Id(id) => {
                        let field = Field::new(None, id, None);
                        value.renew_parent_id(field.clone());
                        return Assign(Accessiable::Field(field), value);
                    },
                    Uni::Index(Index(field, indexes)) => {
                        value.renew_parent_id(field.clone());
                        return Assign(Accessiable::Index(Index(field, indexes)), value)
                    },
                    _ => unreachable!()
                }
            })
    }
}

parser! {
   pub fn return_['a]()(MyStream<'a>) -> Statement
    {
        string_skip_spaces("return")
            .with(skip_spaces(expr_statement()))
            .map(|s| {
                if let StmtKind::Expr(exp) = s.node {
                    Statement::new(StmtKind::Return(exp))
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
                skip_spaces(skip_spaces(word()).and(token_skip_spaces('=').with(skip_spaces(expr()))))
                    .map(|(uni, exp)| Assign(Accessiable::Field(Field::new(None, uni.id(), None)), exp)),
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
            Statement::new(StmtKind::Let(lets, statements))
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
            .map(|exp| Statement::new(StmtKind::Expr(exp)))
    }
}

parser! {
   pub fn expr_statement_no_semicolon['a]()(MyStream<'a>) -> Statement
    {
        skip_spaces(expr()).map(|exp| Statement::new(StmtKind::Expr(exp)))
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
                    (cond, stetements_)
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
                Vec<(Expr, Vec<Statement>)>,
                Option<Vec<Statement>>,
            )| {
                if let Some(else_statement) = else_statement {
                    else_ifs.push((
                        Expr::new(Unary(Uni::Boolean(Boolean::True))),
                        else_statement,
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
                (Expr, Vec<Statement>),
                Vec<(Expr, Vec<Statement>)>,
            )| {
                let mut if_vecs = vec![(cond, stetements_)];
                if_vecs.append(&mut elses);
                Statement::new(StmtKind::If(if_vecs))
            },
        )
    }
}

parser! {
   pub fn assign['a]()(MyStream<'a>) -> Statement
    {
        assign_().map(|assign| Statement::new(StmtKind::Assign(assign)))
    }
}

#[cfg(test)]
mod test {
    use combine::easy;
    use combine::stream::state::{SourcePosition, State};
    use combine::Parser;

    use crate::expr::bin_op::*;
    use crate::statement::*;

    fn helper<'a>(
        input: State<&'a str, SourcePosition>,
    ) -> Result<Statement, easy::Errors<char, &'a str, SourcePosition>> {
        statement().easy_parse(input).map(|r| r.0)
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
            Statement::new(StmtKind::Assign(Assign(
                Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                Expr::new(Unary(Uni::String(String::from("aaa"))))
            )))
        );
    }

    #[test]
    fn return_test() {
        assert_statement!(
            State::new(r#"return "aaa";"#),
            Statement::new(StmtKind::Return(Expr::new(Unary(Uni::String(
                String::from("aaa")
            )))))
        );
    }

    #[test]
    fn if_test() {
        assert_statement!(
            State::new(
                r#"if (i < 10) {
              let abc = "aaa" in
            }"#
            ),
            Statement::new(StmtKind::If(vec![(
                Expr::new(Binary(
                    Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                    BinOpKind::Lt,
                    Box::new(Expr::new(Unary(Uni::Number(10))))
                )),
                vec![Statement::new(StmtKind::Let(
                    vec![Assign(
                        Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                        Expr::new(Unary(Uni::String(String::from("aaa"))))
                    )],
                    vec![],
                ))]
            )]))
        );

        assert_statement!(
            State::new(
                r#"if (i < 10) {
              let abc = "aaa" in
            } else {
              let def = "bbb" in
            }"#
            ),
            Statement::new(StmtKind::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                            Expr::new(Unary(Uni::String(String::from("aaa")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Unary(Uni::Boolean(Boolean::True))),
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("def")), None)),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                )
            ]))
        );

        assert_statement!(
            State::new(
                r#"if (i < 10) {
              let abc = "aaa" in
            } else if (j > 100) {
              let def = "bbb" in
            }"#
            ),
            Statement::new(StmtKind::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
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
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("def")), None)),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                )
            ]))
        );

        assert_statement!(
            State::new(
                r#"if (i < 10) {
              let abc = "aaa" in
            } else if (j > 100) {
              let def = "bbb" in
            } else {
              let ghi = "ccc" in
            }"#
            ),
            Statement::new(StmtKind::If(vec![
                (
                    Expr::new(Binary(
                        Box::new(Expr::new(Unary(Uni::Id(Id(String::from("i")))))),
                        BinOpKind::Lt,
                        Box::new(Expr::new(Unary(Uni::Number(10))))
                    )),
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
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
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("def")), None)),
                            Expr::new(Unary(Uni::String(String::from("bbb")))),
                        )],
                        vec![],
                    ))]
                ),
                (
                    Expr::new(Unary(Uni::Boolean(Boolean::True))),
                    vec![Statement::new(StmtKind::Let(
                        vec![Assign(
                            Accessiable::Field(Field::new(None, Id(String::from("ghi")), None)),
                            Expr::new(Unary(Uni::String(String::from("ccc")))),
                        )],
                        vec![],
                    ))]
                )
            ]))
        );
    }

    #[test]
    fn nest_let_test() {
        assert_statement!(
            State::new(
                r#"let abc = fn(aaa) {
              return aaa;
            } in (
                let def = fn(bbb) {
                  return bbb;
                } in (
                  abc(1) + def(2);
                )
            )"#
            ),
            Statement::new(StmtKind::Let(
                vec![Assign(
                    Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                    Expr::new(Fn(Function(
                        vec![Id(String::from("aaa"))],
                        vec![Statement::new(StmtKind::Return(Expr::new(Unary(Uni::Id(
                            Id(String::from("aaa"))
                        )))))]
                    ))),
                )],
                vec![Statement::new(StmtKind::Let(
                    vec![Assign(
                        Accessiable::Field(Field::new(None, Id(String::from("def")), None)),
                        Expr::new(Fn(Function(
                            vec![Id(String::from("bbb"))],
                            vec![Statement::new(StmtKind::Return(Expr::new(Unary(Uni::Id(
                                Id(String::from("bbb"))
                            )))))]
                        ))),
                    )],
                    vec![Statement::new(StmtKind::Expr(Expr::new(Binary(
                        Box::new(Expr::new(Call(
                            Accessiable::Field(Field::new(None, Id(String::from("abc")), None),),
                            vec![Expr::new(Unary(Uni::Number(1)))]
                        ))),
                        BinOpKind::Add,
                        Box::new(Expr::new(Call(
                            Accessiable::Field(Field::new(None, Id(String::from("def")), None),),
                            vec![Expr::new(Unary(Uni::Number(2)))]
                        ))),
                    )))),]
                ))]
            ))
        );
    }

    #[test]
    fn let_test() {
        assert_statement!(
            State::new(
                r#"let abc = "aaa" in
              abc + "def";
            "#
            ),
            Statement::new(StmtKind::Let(
                vec![Assign(
                    Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                    Expr::new(Unary(Uni::String(String::from("aaa")))),
                )],
                vec![Statement::new(StmtKind::Expr(Expr::new(Binary(
                    Box::new(Expr::new(Unary(Uni::Id(Id(String::from("abc")))))),
                    BinOpKind::Add,
                    Box::new(Expr::new(Unary(Uni::String(String::from("def"))))),
                )))),]
            ))
        );

        assert_statement!(
            State::new(
                r#"let abc = fn(aaa) {
              return aaa + 123;
            } in
              abc(456) + "def";
            "#
            ),
            Statement::new(StmtKind::Let(
                vec![Assign(
                    Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                    Expr::new(Fn(Function(
                        vec![Id(String::from("aaa"))],
                        vec![Statement::new(StmtKind::Return(Expr::new(Binary(
                            Box::new(Expr::new(Unary(Uni::Id(Id(String::from("aaa")))))),
                            BinOpKind::Add,
                            Box::new(Expr::new(Unary(Uni::Number(123)))),
                        ))))]
                    ))),
                )],
                vec![Statement::new(StmtKind::Expr(Expr::new(Binary(
                    Box::new(Expr::new(Call(
                        Accessiable::Field(Field::new(None, Id(String::from("abc")), None),),
                        vec![Expr::new(Unary(Uni::Number(456)))]
                    ))),
                    BinOpKind::Add,
                    Box::new(Expr::new(Unary(Uni::String(String::from("def"))))),
                )))),]
            ))
        );

        assert_statement!(
            State::new(
                r#"let abc = (1 + 3) * 4 in (
              abc = abc + 2;
              return abc;
            )"#
            ),
            Statement::new(StmtKind::Let(
                vec![Assign(
                    Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
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
                    Statement::new(StmtKind::Assign(Assign(
                        Accessiable::Field(Field::new(None, Id(String::from("abc")), None)),
                        Expr::new(Binary(
                            Box::new(Expr::new(Unary(Uni::Id(Id(String::from("abc")))))),
                            BinOpKind::Add,
                            Box::new(Expr::new(Unary(Uni::Number(2)))),
                        ))
                    )),),
                    Statement::new(StmtKind::Return(Expr::new(Unary(Uni::Id(Id(
                        String::from("abc")
                    ))))))
                ],
            ))
        );
    }
}
