use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{attempt, between, choice, many1, parser, sep_by, sep_by1, token, Parser};

#[derive(Debug, PartialEq)]
pub struct Id(pub String);

#[derive(Debug, PartialEq)]
pub enum Uni {
    Id(Id),
    Array(Vec<Uni>),
    String(String),
    Number(i32),
    Boolean(Boolean),
    Field(Vec<Id>),
    HashMap(Vec<(Id, Box<Uni>)>),
}

#[derive(Debug, PartialEq)]
pub enum Boolean {
    True,
    False,
}

pub fn uni_<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();

    choice((attempt(field()).or(word_()), array(), string(), integer())).skip(skip_spaces())
}

#[derive(Debug, PartialEq)]
pub struct HashSet(Id, Box<Uni>);

pub fn hash_set<I>() -> impl Parser<Input = I, Output = HashSet>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    let hash_set_ = || {
        word()
            .skip(skip_spaces())
            .and(token(':'))
            .skip(skip_spaces())
            .and(uni())
            .skip(skip_spaces())
            .map(|((w, _), u)| match w {
                Uni::Id(id) => HashSet(id, Box::new(u)),
                _ => panic!("should come here Id. but actual: {:?}", w),
            })
    };

    attempt(hash_set_().and(token(',')).map(|(h, _)| h)).or(hash_set_())
}

pub fn field<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    sep_by1(word(), token('.'))
        .skip(skip_spaces())
        .map(|mut words: Vec<Uni>| {
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

pub fn word_<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(letter()).map(|e: String| match e.as_ref() {
        "true" => return Uni::Boolean(Boolean::True),
        "false" => return Uni::Boolean(Boolean::False),
        _ => Uni::Id(Id(e.into())),
    })
}

pub fn array<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());
    let comma_list = sep_by(uni(), lex_char(','));
    between(lex_char('['), lex_char(']'), comma_list).map(Uni::Array)
}

pub fn string<I>() -> impl Parser<Input = I, Output = Uni>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let skip_spaces = || spaces().silent();
    let lex_char = |c| char(c).skip(skip_spaces());
    between(lex_char('"'), lex_char('"'), many1(letter())).map(Uni::String)
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
    fn hash_set_test() {
        assert_eq!(
            hash_set().easy_parse(r#"abc: 32"#),
            Ok((HashSet(Id(String::from("abc")), Box::new(Uni::Number(32))), ""))
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
