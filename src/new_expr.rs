use combine::error::ParseError;
use combine::stream::Stream;
use combine::{attempt, between, many, parser, position, sep_by, Parser};
use combine::stream::state::{DefaultPositioned, SourcePosition, State};

use crate::scope::*;
use crate::statement::*;
use crate::utils::*;

use crate::expr::bin_op::{bin_op as bin_op_, BinOpKind};

use crate::expr::uni::{field,  word, Field, Id, Uni};

use crate::pos::{MyStream, uni as create_uni};

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: Id,
    pub node: Node,
}

impl Expr {
    pub fn new(node: Node) -> Expr {
        Expr {
            id: ID_POOL.next_id(),
            node,
        }
    }

    pub fn renew_parent_id(&mut self, id: Id) {
        self.node.renew_parent_id(id);
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Unary(Uni),
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
    Call(Field, Vec<Box<Expr>>),
    Fn(Function),
}

impl Node {
    pub fn renew_parent_id(&mut self, id: Id) {
        match self {
            Node::Binary(left, _, right) => {
                left.renew_parent_id(id.clone());
                right.renew_parent_id(id.clone());
            }
            Node::Unary(unary) => {
                unary.renew_parent_id(id);
            }
            Node::Call(field, _) => {
                field.parent_id = Some(ObjectId(id));
            }
            _ => {}
        };
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function(pub Vec<Id>, pub Vec<Box<Statement>>);

parser! {
   pub fn fn_['a]()(MyStream<'a>) -> Expr
    {
        string_skip_spaces("fn")
            .with(skip_spaces(
                attempt(
                    token_skip_spaces('(')
                        .with(
                            skip_spaces(sep_by(word(), token_skip_spaces(',')))
                                .map(|unis: Vec<Uni>| unis.into_iter().map(|uni| uni.id()).collect()),
                        )
                        .skip(token_skip_spaces(')')),
                )
                .or(token_skip_spaces('(')
                    .and(token_skip_spaces(')'))
                    .map(|_| vec![])),
            ))
            .and(between(
                token_skip_spaces('{'),
                token_skip_spaces('}'),
                many(statement()),
            ))
            .map(|(args, stetements_): (Vec<Id>, Vec<Statement>)| {
                Expr::new(Node::Fn(Function(
                    args,
                    stetements_.into_iter().map(|s| Box::new(s)).collect(),
                )))
            })
    }
}

parser! {
   pub fn expr_['a]()(MyStream<'a>) -> Expr
    {
        attempt(fn_())
            // .or(handle_op(try_paren_with_binary()))
            .or(try_binary())
    }
}

parser! {
   pub fn unary['a]()(MyStream<'a>) -> Expr
    {
        attempt(call()).or(create_uni().map(|uni| Expr::new(Node::Unary(uni))))
    }
}

parser! {
   pub fn try_paren_with_binary['a]()(MyStream<'a>) -> Expr
    {
        between(token_skip_spaces('('), token_skip_spaces(')'), try_binary())
    }
}


// pub fn handle_op<I>(
//     input: impl Parser<Input = I, Output = Expr>,
// ) -> impl Parser<Input = I, Output = Expr>
// where
//     I: Stream<Item = char>,
//     I::Error: ParseError<I::Item, I::Range, I::Position>,
// {
//     skip_spaces(input)
//         .and(skip_spaces(
//             attempt(many(skip_spaces(bin_op_()).and(unary())))
//                 .or(many(skip_spaces(bin_op_()).and(expr()))),
//         ))
//         .map(|(left, mut right_pairs): (Expr, Vec<(BinOpKind, Expr)>)| {
//             match right_pairs.len() {
//                 0 => return left,
//                 1 => {
//                     let (bin_op, right) = right_pairs.remove(0);
//                     return Expr::new(Node::Binary(Box::new(left), bin_op, Box::new(right)));
//                 }
//                 _ => {
//                     let mut exp = left;
//                     let mut left_pair = right_pairs.remove(0);
//                     let mut right_pair = right_pairs.remove(0);
//                     let mut length = right_pairs.len();
//                     loop {
//                         let left_priority = left_pair.0.priority();
//                         let right_priority = right_pair.0.priority();

//                         // [exp1 op1 exp2] op2 exp3
//                         if left_priority > right_priority {
//                             let (left_op, left_exp) = left_pair;
//                             exp =
//                                 Expr::new(Node::Binary(Box::new(exp), left_op, Box::new(left_exp)));

//                             if length == 0 {
//                                 let (right_op, right_exp) = right_pair;
//                                 return Expr::new(Node::Binary(
//                                     Box::new(exp),
//                                     right_op,
//                                     Box::new(right_exp),
//                                 ));
//                             }

//                             left_pair = right_pair;
//                             right_pair = right_pairs.remove(0);
//                         }
//                         // exp1 op1 [exp2 op2 exp3]
//                         else {
//                             let (left_op, left_exp) = left_pair;
//                             let (right_op, right_exp) = right_pair;
//                             left_pair = (
//                                 left_op,
//                                 Expr::new(Node::Binary(
//                                     Box::new(left_exp),
//                                     right_op,
//                                     Box::new(right_exp),
//                                 )),
//                             );

//                             if length == 0 {
//                                 let (left_op, left_exp) = left_pair;
//                                 return Expr::new(Node::Binary(
//                                     Box::new(exp),
//                                     left_op,
//                                     Box::new(left_exp),
//                                 ));
//                             }

//                             right_pair = right_pairs.remove(0);
//                         }
//                         length -= 1;
//                     }
//                 }
//             }
//         })
// }

parser! {
   pub fn try_binary['a]()(MyStream<'a>) -> Expr
    {
        // attempt(handle_op(unary())).or(unary())
        unary()
    }
}

parser! {
   pub fn call['a]()(MyStream<'a>) -> Expr
    {
        skip_spaces(field())
            .and(between(
                token_skip_spaces('('),
                token_skip_spaces(')'),
                skip_spaces(sep_by(skip_spaces(expr()), token_skip_spaces(',')))
                    .map(|exps: Vec<Expr>| exps.into_iter().map(|exp| Box::new(exp)).collect()),
            ))
            .map(|(fn_name, args)| match fn_name {
                Uni::Id(id) => Expr::new(Node::Call(Field::new(None, id, None), args)),
                Uni::Field(field) => Expr::new(Node::Call(field, args)),
                _ => panic!("should come Uni::Id. actual: {:?}", fn_name),
            })
    }
}

parser! {
   pub fn expr['a]()(MyStream<'a>) -> Expr
    {
        expr_()
        .and(position())
        .map(|(name, pos): (Expr, SourcePosition)|{
            name
        })
    }
}