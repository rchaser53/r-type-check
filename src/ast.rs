use combine::char::spaces;
use combine::error::ParseError;
use combine::stream::Stream;
use combine::{many, Parser};

use crate::expr::uni::*;
use crate::statement::*;

pub fn ast<I>() -> impl Parser<Input = I, Output = Vec<Statement>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().with(many(statement()))
}

mod test {
    use crate::ast::*;
    use crate::expr::bin_op::*;
    use crate::expr::*;
    use crate::statement::Assign;
    use crate::statement::Statement;

    #[test]
    fn assigns_test() {
        let input = r#"
abc = 123;
def = 456;"#;
        assert_eq!(
            ast().easy_parse(input),
            Ok((
                vec![
                    Statement::Assign(Assign(
                        Id(String::from("abc")),
                        Expr::Unary(Uni::Number(123))
                    )),
                    Statement::Assign(Assign(
                        Id(String::from("def")),
                        Expr::Unary(Uni::Number(456))
                    )),
                ],
                ""
            ))
        );
    }

    #[test]
    fn lets_test() {
        let input = r#"
let abc = 123 in
def = 456;"#;
        assert_eq!(
            ast().easy_parse(input),
            Ok((
                vec![Statement::Let(
                    Id(String::from("abc")),
                    Expr::Unary(Uni::Number(123)),
                    vec![Box::new(Statement::Assign(Assign(
                        Id(String::from("def")),
                        Expr::Unary(Uni::Number(456))
                    )))]
                ),],
                ""
            ))
        );
    }
}
