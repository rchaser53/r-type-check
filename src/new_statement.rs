use combine::parser::choice::optional;
use combine::{attempt, between, choice, many, parser};

use crate::expr::uni::*;
use crate::expr::Node::*;
use crate::expr::*;
use crate::pos::MyStream;
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
