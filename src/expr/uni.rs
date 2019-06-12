use combine::error::ParseError;
use combine::parser::char::{digit, letter};
use combine::stream::Stream;
use combine::{attempt, between, choice, many, many1, none_of, parser, sep_by, sep_by1, Parser};

use crate::utils::{skip_spaces, string_skip_spaces, token_skip_spaces};

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
    Field(Vec<Id>),
    HashMap(Vec<HashSet>),
    Null,
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

#[derive(Clone, Debug, PartialEq)]
pub struct HashSet(Id, Box<Uni>);

pub fn uni_<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_spaces(choice((
        attempt(field()).or(word_()),
        hash_map(),
        array(),
        string(),
        integer(),
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
        .map(|hs| Uni::HashMap(hs))
}

pub fn hash_set<I>() -> impl Parser<Input = I, Output = HashSet>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let hash_set_ = || {
        skip_spaces(word())
            .and(token_skip_spaces(':'))
            .and(skip_spaces(uni()))
            .map(|((w, _), u)| match w {
                Uni::Id(id) => HashSet(id, Box::new(u)),
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
            let fields = words
                .into_iter()
                .map(|word| match word {
                    Uni::Id(id) => id,
                    _ => panic!("should come here Id. but actual: {:?}", word),
                })
                .collect();
            Uni::Field(fields)
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
    attempt(preserved()).or(many1(letter()).map(|e: String| Uni::Id(Id(e.into()))))
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

    #[test]
    fn hash_map_test() {
        assert_eq!(
            uni().easy_parse(
                r#"{
            }"#
            ),
            Ok((Uni::HashMap(vec![]), ""))
        );

        assert_eq!(
            uni().easy_parse(
                r#"{
              abc: 32
            }"#
            ),
            Ok((
                Uni::HashMap(vec![HashSet(
                    Id(String::from("abc")),
                    Box::new(Uni::Number(32))
                ),]),
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
                Uni::HashMap(vec![
                    HashSet(Id(String::from("abc")), Box::new(Uni::Number(32))),
                    HashSet(
                        Id(String::from("def")),
                        Box::new(Uni::String(String::from("def_value!")))
                    ),
                ]),
                ""
            ))
        );
    }

    #[test]
    fn hash_set_test() {
        assert_eq!(
            hash_set().easy_parse(r#"abc: 32"#),
            Ok((
                HashSet(Id(String::from("abc")), Box::new(Uni::Number(32))),
                ""
            ))
        );
    }

    #[test]
    fn boolean_test() {
        assert_eq!(
            uni().easy_parse(r#"true"#),
            Ok((Uni::Boolean(Boolean::True), ""))
        );
    }

    #[test]
    fn number_test() {
        assert_eq!(uni().easy_parse(r#"123"#), Ok((Uni::Number(123), "")));
    }

    #[test]
    fn field_test() {
        assert_eq!(
            uni().easy_parse(r#"abc.def"#),
            Ok((
                Uni::Field(vec![Id(String::from("abc")), Id(String::from("def")),]),
                ""
            ))
        );
    }
}
