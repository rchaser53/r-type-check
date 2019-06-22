use combine::easy;
use combine::stream::state::{DefaultPositioned, State};

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;
