use ast::Value;
use scope::Scope;
use std::rc::Rc;

macro_rules! args_to_ref {
    ( $a:expr ) => {{
        $a.iter().map(|a| a.as_ref()).collect::<Vec<&Value>>()
    }}
}

fn add(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Int(l), &Value::Int(r)] => Value::Int(l + r),
        [ref a..] => panic!("Invalid args to add: {:?}", a),
    }
}

macro_rules! cmp_branches {
    ( $n:ident, $e:expr, $op:ident, $( $t:path ),* ) => {{
        match args_to_ref!($e)[..] {
            $(
                [&$t(ref l), &$t(ref r)] => Value::Bool(l.$op(r)),
            )*
            [ref a..] => panic!("Invalid args to $n: {:?}", a),
        }
    }};
}

fn eq_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(equal,
                  args,
                  eq,
                  Value::Bool,
                  Value::Int,
                  Value::Str,
                  Value::Vec,
                  Value::Fn,
                  Value::PrimitiveFn)
}

fn gt_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(greater_than, args, gt, Value::Int, Value::Str, Value::Vec)
}

fn ge_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(greater_equal, args, ge, Value::Int, Value::Str, Value::Vec)
}

fn lt_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(less_than, args, lt, Value::Int, Value::Str, Value::Vec)
}

fn le_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(less_equal, args, le, Value::Int, Value::Str, Value::Vec)
}

fn get(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Vec(ref l), &Value::Int(i)] => {
            match l.get(i as usize) {
                Some(v) => v.as_ref().clone(),
                None => Value::Nil,
            }
        }
        [&Value::Map(ref m), v] => {
            match m.get(v) {
                Some(v) => v.as_ref().clone(),
                None => Value::Nil,
            }
        }
        [ref a..] => panic!("Invalid args to get: {:?}", a),
    }
}

fn len(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Vec(ref l)] => Value::Int(l.len() as i64),
        [&Value::Map(ref m)] => Value::Int(m.len() as i64),
        [ref a..] => panic!("Invalid args to len: {:?}", a),
    }
}

fn keys(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Vec(ref l)] => {
            Value::Vec((0..l.len())
                .map(|i| Rc::new(Value::Int(i as i64)))
                .collect::<Vec<Rc<Value>>>())
        }
        [&Value::Map(ref m)] => Value::Vec(m.keys().map(|k| k.clone()).collect::<Vec<Rc<Value>>>()),
        [ref a..] => panic!("Invalid args to len: {:?}", a),
    }
}

fn push(mut args: Vec<Rc<Value>>) -> Value {
    assert!(args.len() == 2, "Invalid args to push: {:?}", args);
    let val = args.pop().unwrap();
    let list = match Rc::try_unwrap(args.pop().unwrap()) {
        Ok(l) => l,
        Err(rc) => rc.as_ref().clone(),
    };

    match (list, val) {
        (Value::Vec(mut l), v) => {
            l.push(v);
            Value::Vec(l)
        }
        (l, v) => panic!("Invalid args to push: {:?} {:?}", l, v),
    }
}

fn insert(mut args: Vec<Rc<Value>>) -> Value {
    assert!(args.len() == 3, "Invalid args to insert: {:?}", args);
    let val = args.pop().unwrap();
    let key = args.pop().unwrap();
    let map = match Rc::try_unwrap(args.pop().unwrap()) {
        Ok(m) => m,
        Err(rc) => rc.as_ref().clone(),
    };

    match (map, key, val) {
        (Value::Map(mut m), k, v) => {
            m.insert(k, v);
            Value::Map(m)
        }
        (m, k, v) => panic!("Invalid args to insert: {:?} {:?} {:?}", m, k, v),
    }
}

fn print_pfn(args: Vec<Rc<Value>>) -> Value {
    println!("stdout >> {:?}", args);
    Value::Nil
}

fn add_to_scope(scope: &mut Scope, key: &str, primitive: fn(Vec<Rc<Value>>) -> Value) {
    scope.insert(key.to_string(), Rc::new(Value::PrimitiveFn(primitive)))
}

pub fn if_pfn_marker(_: Vec<Rc<Value>>) -> Value {
    unreachable!()
}

pub fn while_pfn_marker(_: Vec<Rc<Value>>) -> Value {
    unreachable!()
}

pub fn add_primitive_fns(scope: &mut Scope) {
    add_to_scope(scope, "if", if_pfn_marker);
    add_to_scope(scope, "while", while_pfn_marker);

    add_to_scope(scope, "add", add);
    add_to_scope(scope, "get", get);
    add_to_scope(scope, "insert", insert);
    add_to_scope(scope, "keys", keys);
    add_to_scope(scope, "len", len);
    add_to_scope(scope, "push", push);
    add_to_scope(scope, "print", print_pfn);

    add_to_scope(scope, "eq", eq_pfn);
    add_to_scope(scope, "gt", gt_pfn);
    add_to_scope(scope, "ge", ge_pfn);
    add_to_scope(scope, "lt", lt_pfn);
    add_to_scope(scope, "le", le_pfn);
}
