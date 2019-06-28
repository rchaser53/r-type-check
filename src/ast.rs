use combine::char::spaces;
use combine::many;

use crate::pos::MyStream;
use crate::statement::*;

parser! {
   pub fn ast['a]()(MyStream<'a>) -> Vec<Statement>
    {
        spaces().with(many(statement()))
    }
}

#[cfg(test)]
mod test {
    use combine::stream::state::State;
    use combine::Parser;

    use crate::ast::*;
    use crate::expr::uni::*;
    use crate::expr::Node::*;
    use crate::expr::*;
    use crate::statement::Assign;
    use crate::statement::Statement;

    #[test]
    fn assigns_test() {
        let input = State::new(
            r#"
abc = 123;
def = 456;"#,
        );
        assert_eq!(
            ast().easy_parse(input).unwrap().0,
            vec![
                Statement::new(StmtKind::Assign(Assign(
                    Assignable::Field(Field::new(None, Id(String::from("abc")), None)),
                    Expr::new(Unary(Uni::Number(123)))
                ))),
                Statement::new(StmtKind::Assign(Assign(
                    Assignable::Field(Field::new(None, Id(String::from("def")), None)),
                    Expr::new(Unary(Uni::Number(456)))
                ))),
            ],
        );
    }

    #[test]
    fn lets_test() {
        let input = State::new(
            r#"
let abc = 123 in
def = 456;"#,
        );
        assert_eq!(
            ast().easy_parse(input).unwrap().0,
            vec![Statement::new(StmtKind::Let(
                vec![Assign(
                    Assignable::Field(Field::new(None, Id(String::from("abc")), None)),
                    Expr::new(Unary(Uni::Number(123))),
                )],
                vec![Statement::new(StmtKind::Assign(Assign(
                    Assignable::Field(Field::new(None, Id(String::from("def")), None)),
                    Expr::new(Unary(Uni::Number(456)))
                )))]
            )),],
        );
    }
}
