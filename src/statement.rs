use combine::error::ParseError;
use combine::parser::choice::optional;
use combine::stream::Stream;
use combine::{attempt, between, choice, many, parser, Parser};

use crate::expr::uni::*;
use crate::expr::Node::*;
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

pub fn if_condition<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(token_skip_spaces('('), token_skip_spaces(')'), expr()).map(|expr| expr)
}

pub fn assign_<I>() -> impl Parser<Input = I, Output = Assign>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
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

pub fn return_<I>() -> impl Parser<Input = I, Output = Statement>
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

pub fn let_<I>() -> impl Parser<Input = I, Output = Statement>
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

pub fn expr_statement<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(expr_statement_with_semicolon()).or(expr_statement_no_semicolon())
}

pub fn expr_statement_with_semicolon<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(expr())
        .skip(token_skip_spaces(';'))
        .map(|exp| Statement::Expr(exp))
}

pub fn expr_statement_no_semicolon<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(expr()).map(|exp| Statement::Expr(exp))
}

pub fn if_<I>() -> impl Parser<Input = I, Output = Statement>
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

pub fn else_if_<I>() -> impl Parser<Input = I, Output = Vec<(Expr, Vec<Box<Statement>>)>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many(attempt(create_if(
        string_skip_spaces("else").with(string_skip_spaces("if")),
    )))
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
}

pub fn create_if<I, T>(
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

pub fn assign<I>() -> impl Parser<Input = I, Output = Statement>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    assign_().map(|assign| Statement::Assign(assign))
}
