use std::rc::Rc;
use std::collections::BTreeMap;

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Vec(Vec<Rc<Value>>),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Fn(Box<(String, Vec<String>, Vec<Expression>)>),
    RawFn(Box<(Vec<String>, Vec<Expression>)>),
    PrimitiveFn(fn(Vec<Rc<Value>>) -> Value),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    Assign(String, Box<Expression>),
    Block(Vec<Expression>),
    Call(String, Vec<Expression>),
    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Symbol(String),
    Value(Value),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Nil,
    Bool,
    Int,
    Str,
    Vec(Box<Type>),
    Map(Box<(Type, Type)>),
    Fn(Box<(Vec<Type>, Type)>)
}
