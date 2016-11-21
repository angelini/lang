#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Vec(Vec<Value>),
    Fn(Box<(String, Vec<String>, Vec<Expression>)>),
    RawFn(Box<(Vec<String>, Vec<Expression>)>),
    PrimitiveFn(fn(Vec<Value>) -> Value),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expression {
    Assign(String, Box<Expression>),
    Block(Vec<Expression>),
    Call(String, Vec<Expression>),
    List(Vec<Expression>),
    Symbol(String),
    Value(Value),
}
