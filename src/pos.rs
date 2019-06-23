use combine::easy;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub hi: SourcePosition,
    pub lo: SourcePosition,
}

impl Default for Position {
    fn default() -> Self {
        Position {
            hi: SourcePosition::new(),
            lo: SourcePosition::new(),
        }
    }
}
