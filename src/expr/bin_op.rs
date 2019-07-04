use std::fmt;

use combine::error::ParseError;
use combine::parser::char::string;
use combine::stream::Stream;
use combine::{choice, parser, Parser};

#[derive(Clone, Copy, PartialEq)]
pub enum BinOpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Shr, // %
    Eq,  // ==
    Lt,  // <
    Le,  // <=
    Ne,  // !=
    Ge,  // >=
    Gt,  // >
}

impl BinOpKind {
    pub fn priority(self) -> usize {
        match self {
            BinOpKind::Add => 2,
            BinOpKind::Sub => 2,
            BinOpKind::Mul => 3,
            BinOpKind::Div => 3,
            BinOpKind::Shr => 3,
            BinOpKind::Eq => 1,
            BinOpKind::Lt => 1,
            BinOpKind::Le => 1,
            BinOpKind::Ne => 1,
            BinOpKind::Ge => 1,
            BinOpKind::Gt => 1,
        }
    }
}

impl fmt::Debug for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
            BinOpKind::Shr => "%",
            BinOpKind::Eq => "==",
            BinOpKind::Lt => "<",
            BinOpKind::Le => "<=",
            BinOpKind::Ne => "!=",
            BinOpKind::Ge => ">=",
            BinOpKind::Gt => ">",
        };
        write!(f, "{}", result)
    }
}

pub fn bin_op_<I>() -> impl Parser<Input = I, Output = BinOpKind>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        add(),
        sub(),
        mul(),
        div(),
        shr(),
        eq(),
        ne(),
        lt(),
        le(),
        gt(),
        ge(),
    ))
}

macro_rules! create_op {
    ($name:ident, $str:expr, $op:expr) => {
        fn $name<I>() -> impl Parser<Input = I, Output = BinOpKind>
        where
            I: Stream<Item = char>,
            I::Error: ParseError<I::Item, I::Range, I::Position>,
        {
            string($str).map(|_| $op)
        }
    };
}

create_op!(add, "+", BinOpKind::Add);
create_op!(sub, "-", BinOpKind::Sub);
create_op!(mul, "*", BinOpKind::Mul);
create_op!(div, "/", BinOpKind::Div);
create_op!(shr, "%", BinOpKind::Shr);

create_op!(eq, "==", BinOpKind::Eq);
create_op!(ne, "!=", BinOpKind::Ne);
create_op!(lt, "<", BinOpKind::Lt);
create_op!(le, "<=", BinOpKind::Le);
create_op!(gt, ">", BinOpKind::Gt);
create_op!(ge, ">=", BinOpKind::Ge);

parser! {
    pub fn bin_op[I]()(I) -> BinOpKind
    where [I: Stream<Item = char>]
    {
        bin_op_()
    }
}
