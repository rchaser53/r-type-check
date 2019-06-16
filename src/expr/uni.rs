use std::collections::HashMap;

use combine::error::ParseError;
use combine::parser::char::{digit, letter};
use combine::stream::Stream;
use combine::{
    attempt, between, choice, many, many1, none_of, parser, sep_by, sep_by1, token, Parser,
};

use crate::expr::*;

#[derive(Clone, Debug, Hash)]
pub struct Id(pub String);
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for Id {}

#[derive(Clone, Debug, PartialEq)]
pub enum Uni {
    Id(Id),
    Array(Vec<Uni>),
    String(String),
    Number(i32),
    Boolean(Boolean),
    Field(Field),
    HashMap(Hash),
    Null,
}

impl Uni {
    pub fn renew_parent_id(&mut self, id: Id) {
        match self {
            Uni::Field(field) => {
                field.parent_id = Some(ObjectId(id));
            }
            _ => {}
        };
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub parent_id: Option<ObjectId>,
    pub id: ObjectId,
    pub child: Option<Box<Field>>,
}
impl Field {
    pub fn new(parent_id: Option<ObjectId>, id: Id, child: Option<Box<Field>>) -> Self {
        Field {
            parent_id,
            id: ObjectId(id),
            child,
        }
    }

    pub fn attach(&mut self, child: Field) {
        self.child = Some(Box::new(child));
    }
}

impl Uni {
    pub fn to_string(&self) -> String {
        match self {
            Uni::Id(id) => id.0.to_string(),
            Uni::Array(array) => array
                .into_iter()
                .map(|uni| uni.to_string())
                .collect::<Vec<String>>()
                .join(","),
            Uni::String(string_) => string_.clone(),
            Uni::Number(num) => num.to_string(),
            Uni::Boolean(boolean) => boolean.to_string(),
            Uni::Field(_) => "field".to_string(),
            Uni::HashMap(_) => "hash_map".to_string(),
            Uni::Null => "null".to_string(),
        }
    }

    pub fn id(&self) -> Id {
        match self {
            Uni::Id(id) => id.clone(),
            _ => panic!("should use id only Uni::Id. actual:{:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Boolean {
    True,
    False,
}

impl Boolean {
    pub fn to_string(&self) -> String {
        match self {
            Boolean::True => "true".to_string(),
            Boolean::False => "false".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Hash(pub Id, pub HashMap<Id, Box<Expr>>);
impl PartialEq for Hash {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

pub type HashSet = (Id, Box<Expr>);

pub fn uni_<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(choice((
        attempt(field()),
        attempt(hash_map()),
        attempt(array()),
        attempt(string()),
        attempt(integer()),
    )))
}

pub fn hash_map<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    token_skip_spaces('{')
        .with(skip_spaces(many(hash_set())))
        .skip(token_skip_spaces('}'))
        .map(|hs: Vec<HashSet>| {
            let mut hash_map = HashMap::new();
            for (id, boxed_exp) in hs.into_iter() {
                hash_map.insert(id, boxed_exp);
            }
            Uni::HashMap(Hash(ID_POOL.next_id(), hash_map))
        })
}

pub fn hash_set<I>() -> impl Parser<Input = I, Output = HashSet>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let hash_set_ = || {
        skip_spaces(word())
            .and(token_skip_spaces(':'))
            .and(skip_spaces(expr()))
            .map(|((w, _), u)| match w {
                Uni::Id(id) => (id, Box::new(u)),
                _ => panic!("should come here Id. but actual: {:?}", w),
            })
    };

    attempt(hash_set_().skip(token_skip_spaces(','))).or(skip_spaces(hash_set_()))
}

pub fn field<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(sep_by1(word(), token_skip_spaces('.'))).map(|mut words: Vec<Uni>| {
        let length = words.len();
        if length > 1 {
            if let Some(result) = words
                .into_iter()
                .fold(None, |previous: Option<Field>, next| {
                    let id = match next {
                        Uni::Id(id) => id,
                        _ => unreachable!(),
                    };

                    fn set_field_to_leaf(mut field: Field, id: Id) -> Field {
                        if field.child.is_none() {
                            field.child =
                                Some(Box::new(Field::new(Some(field.id.clone()), id, None)));
                            field
                        } else {
                            set_field_to_leaf(*field.child.unwrap(), id)
                        }
                    }

                    let result = if let Some(previous) = previous {
                        set_field_to_leaf(previous, id)
                    } else {
                        Field::new(None, id, None)
                    };

                    Some(result)
                })
            {
                Uni::Field(result)
            } else {
                panic!("uni field: should not come here");
            }
        } else {
            words.pop().unwrap()
        }
    })
}

pub fn preserved<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(string_skip_spaces("true").map(|_| Uni::Boolean(Boolean::True)))
        .or(string_skip_spaces("false").map(|_| Uni::Boolean(Boolean::False)))
        .or(string_skip_spaces("null").map(|_| Uni::Null))
}

pub fn word_<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(preserved())
        .or(many1(attempt(letter()).or(token('_'))).map(|e: String| Uni::Id(Id(e.into()))))
}

pub fn array<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let comma_list = sep_by(uni(), token_skip_spaces(','));
    between(token_skip_spaces('['), token_skip_spaces(']'), comma_list).map(Uni::Array)
}

pub fn string<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        token_skip_spaces('"'),
        token_skip_spaces('"'),
        many(none_of("\"".chars())),
    )
    .map(Uni::String)
}

pub fn integer<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(digit())
        .map(|string: String| string.parse::<i32>().unwrap())
        .map(Uni::Number)
}

parser! {
    pub fn word[I]()(I) -> Uni
    where [I: Stream<Item = char>]
    {
        word_()
    }
}

parser! {
    pub fn uni[I]()(I) -> Uni
    where [I: Stream<Item = char>]
    {
        uni_()
    }
}

mod test {
    use crate::expr::uni::*;

    fn create_hash_map(input: &[(Id, Box<Expr>)]) -> HashMap<Id, Box<Expr>> {
        input.iter().cloned().collect()
    }

    #[test]
    fn hash_map_test() {
        assert_eq!(
            uni().easy_parse(
                r#"{
            }"#
            ),
            Ok((Uni::HashMap(Hash(ID_POOL.next_id(), HashMap::new())), ""))
        );

        assert_eq!(
            uni().easy_parse(
                r#"{
              abc: 32
            }"#
            ),
            Ok((
                Uni::HashMap(Hash(
                    ID_POOL.next_id(),
                    create_hash_map(&[(
                        Id(String::from("abc")),
                        Box::new(Expr::new(Node::Unary(Uni::Number(32)))),
                    )])
                )),
                ""
            ))
        );

        assert_eq!(
            uni().easy_parse(
                r#"{
              abc: 32,
              def: "def_value!",
            }"#
            ),
            Ok((
                Uni::HashMap(Hash(
                    ID_POOL.next_id(),
                    create_hash_map(&[
                        (
                            Id(String::from("abc")),
                            Box::new(Expr::new(Node::Unary(Uni::Number(32)))),
                        ),
                        (
                            Id(String::from("def")),
                            Box::new(Expr::new(Node::Unary(Uni::String(String::from(
                                "def_value!",
                            ))))),
                        ),
                    ])
                )),
                ""
            ))
        );

        assert_eq!(
            uni().easy_parse(
                r#"{
              abc: fn() {
                return 3;
              },
              def: "def_value!",
            }"#
            ),
            Ok((
                Uni::HashMap(Hash(
                    ID_POOL.next_id(),
                    create_hash_map(&[
                        (
                            Id(String::from("abc")),
                            Box::new(Expr::new(Node::Fn(Function(
                                vec![],
                                vec![Box::new(Statement::Return(Expr::new(Node::Unary(
                                    Uni::Number(3),
                                ))))],
                            )))),
                        ),
                        (
                            Id(String::from("def")),
                            Box::new(Expr::new(Node::Unary(Uni::String(String::from(
                                "def_value!",
                            ))))),
                        ),
                    ])
                )),
                ""
            ))
        );
    }

    #[test]
    fn hash_map_nest() {
        assert_eq!(
            uni().easy_parse(
                r#"{
              abc: {
                inner_abc: 12
              },
              def: "def_value!",
            }"#
            ),
            Ok((
                Uni::HashMap(Hash(
                    ID_POOL.next_id(),
                    create_hash_map(&[
                        (
                            Id(String::from("abc")),
                            Box::new(Expr::new(Node::Unary(Uni::HashMap(Hash(
                                ID_POOL.next_id(),
                                create_hash_map(&[(
                                    Id(String::from("inner_abc")),
                                    Box::new(Expr::new(Node::Unary(Uni::Number(12)))),
                                )])
                            ))))),
                        ),
                        (
                            Id(String::from("def")),
                            Box::new(Expr::new(Node::Unary(Uni::String(String::from(
                                "def_value!",
                            ))))),
                        ),
                    ])
                )),
                ""
            ))
        );
    }

    #[test]
    fn field_test() {
        assert_eq!(
            uni().easy_parse(r#"abc.def"#),
            Ok((
                Uni::Field(Field::new(
                    None,
                    Id(String::from("abc")),
                    Some(Box::new(Field::new(
                        Some(ObjectId(Id(String::from("abc")))),
                        Id(String::from("def")),
                        None
                    )))
                )),
                ""
            ))
        );
    }
}
