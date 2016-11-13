#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Str(String),
    Vec(Vec<Value>),
    Fn(Box<(Vec<String>, Vec<Expression>)>),
    PrimitiveFn(Box<fn(Vec<Value>) -> Value>),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Call(String, Vec<Expression>),
    Assign(String, Box<Expression>),
    List(Vec<Expression>),
    Value(Value),
    Symbol(String),
}
