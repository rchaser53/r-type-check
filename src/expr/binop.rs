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
