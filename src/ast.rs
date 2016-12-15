use std::fmt;
use std::rc::Rc;
use std::collections::BTreeMap;

pub type PrimitiveFn = fn(Vec<Rc<Value>>) -> Value;
pub type PrimitiveFnTypes = (Vec<Type>, Type);
pub type TypedIdentifier = (String, Type);

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Unknown,
    Nil,
    Bool,
    Int,
    Str,
    List(Box<Type>),
    Tuple(Vec<Type>),
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
    Tuple(Vec<Rc<Value>>),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Fn(Box<(String, Vec<TypedIdentifier>, Vec<Expression>)>),
    PrimitiveFn(Box<(String, PrimitiveFn)>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", if v { "true" } else { "false" }),
            Value::Int(v) => write!(f, "{}", v.to_string()),
            Value::Str(ref v) => write!(f, "\"{}\"", v),
            Value::List(ref v) => {
                let items = v.iter().map(|i| format!("{}", i)).collect::<Vec<String>>();
                write!(f, "[{}]", items.join(", "))
            }
            Value::Tuple(ref v) => {
                let items = v.iter().map(|i| format!("{}", i)).collect::<Vec<String>>();
                write!(f, "({})", items.join(", "))
            }
            Value::Map(ref v) => {
                let items = v.iter().map(|(k, v)| format!("{}: {}", k, v)).collect::<Vec<String>>();
                write!(f, "{{{}}}", items.join(", "))
            }
            Value::Fn(box (ref key, _, _)) => write!(f, "fn<{}>", key),
            Value::PrimitiveFn(box (ref sym, _)) => write!(f, "primitivefn<{}>", sym),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    Assign(Box<(bool, String, Option<Type>, Expression)>),
    Block(Vec<Expression>),
    Call(String, Vec<Expression>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Fn(Vec<(String, Type)>, Vec<Expression>),
    Symbol(String),
    Value(Value),
}
