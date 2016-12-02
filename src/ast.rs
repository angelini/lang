use std::rc::Rc;
use std::collections::BTreeMap;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Unknown,
    Nil,
    Bool,
    Int,
    Str,
    List(Box<Type>),
    Map(Box<(Type, Type)>),
    Fn(Box<(Vec<Type>, Type)>),
    Var(String),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    List(Vec<Rc<Value>>),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Fn(Box<(String, Vec<(String, Type)>, Vec<Expression>)>),
    PrimitiveFn(Box<(String, fn(Vec<Rc<Value>>) -> Value)>),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    Assign(Box<(bool, String, Option<Type>, Expression)>),
    Block(Vec<Expression>),
    Call(String, Vec<Expression>),
    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Fn(Vec<(String, Type)>, Vec<Expression>),
    Symbol(String),
    Value(Value),
}
