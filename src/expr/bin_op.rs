use combine::error::ParseError;
use combine::parser::char::string;
use combine::stream::Stream;
use combine::{choice, parser, Parser};

#[derive(Debug, PartialEq)]
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

mod test {
    use crate::expr::bin_op::*;

    #[test]
    fn bin_op_test() {
        assert_eq!(bin_op().parse(r#"+"#), Ok((BinOpKind::Add, "")));
    }
}
