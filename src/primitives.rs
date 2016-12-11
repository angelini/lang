use ast::{PrimitiveFn, PrimitiveFnTypes, Type, Value};
use scope::{self, TypeScope, ValueScope};
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

fn sub(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Int(l), &Value::Int(r)] => Value::Int(l - r),
        [ref a..] => panic!("Invalid args to sub: {:?}", a),
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
                  Value::List,
                  Value::Fn,
                  Value::PrimitiveFn)
}

fn gt_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(greater_than, args, gt, Value::Int, Value::Str, Value::List)
}

fn ge_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(greater_equal, args, ge, Value::Int, Value::Str, Value::List)
}

fn lt_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(less_than, args, lt, Value::Int, Value::Str, Value::List)
}

fn le_pfn(args: Vec<Rc<Value>>) -> Value {
    cmp_branches!(less_equal, args, le, Value::Int, Value::Str, Value::List)
}

fn get(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::List(ref l), &Value::Int(i)] => {
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

macro_rules! tuple_get {
    ( $i:expr, $e:expr ) => {{
        let fn_name = format!("t{}", $i);
        match args_to_ref!($e)[..] {
            [&Value::Tuple(ref t)] => {
                match t.get($i) {
                    Some(v) => v.as_ref().clone(),
                    None => panic!("Invalid args to {}: {:?}", fn_name, Value::Tuple(t.clone())),
                }
            }
            [ref a..] => panic!("Invalid args to {}: {:?}", fn_name, a),
        }
    }};
}

fn t0(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(0, args)
}

fn t1(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(1, args)
}

fn t2(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(2, args)
}

fn t3(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(3, args)
}

fn t4(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(4, args)
}

fn t5(args: Vec<Rc<Value>>) -> Value {
    tuple_get!(5, args)
}

fn size(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::List(ref l)] => Value::Int(l.len() as i64),
        [&Value::Map(ref m)] => Value::Int(m.len() as i64),
        [ref a..] => panic!("Invalid args to size: {:?}", a),
    }
}

fn keys(args: Vec<Rc<Value>>) -> Value {
    match args_to_ref!(args)[..] {
        [&Value::Map(ref m)] => Value::List(m.keys().cloned().collect::<Vec<Rc<Value>>>()),
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
        (Value::List(mut l), v) => {
            l.push(v);
            Value::List(l)
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
    match args_to_ref!(args)[..] {
        [&Value::Tuple(ref values)] => {
            println!("stdout >> {}",
                     values.iter().map(|v| format!("{:?}", v)).collect::<Vec<String>>().join(", "))
        }
        [v] => println!("stdout >> {:?}", v),
        [ref a..] => panic!("Invalid args to print: {:?}", a),
    }
    Value::Nil
}

pub fn if_pfn_marker(_: Vec<Rc<Value>>) -> Value {
    unreachable!()
}

pub fn while_pfn_marker(_: Vec<Rc<Value>>) -> Value {
    unreachable!()
}

pub fn add_primitive_fns(tscope: &mut TypeScope,
                         vscope: &mut ValueScope)
                         -> Result<(), scope::Error> {
    fn type_var(s: &str) -> Type {
        Type::Var(s.to_string())
    }

    fn map_type() -> Type {
        Type::Map(box (type_var("k"), type_var("v")))
    }

    fn list_type(s: &str) -> Type {
        Type::List(box type_var(s))
    }

    fn tuple_type(keys: &[&str]) -> Type {
        Type::Tuple(keys.iter().map(|k| type_var(k)).collect())
    }

    let primitives: Vec<(&str, PrimitiveFn, PrimitiveFnTypes)> = vec![
        ("if", if_pfn_marker, (vec![Type::Bool, type_var("t"), type_var("t")], type_var("t"))),
        ("while", while_pfn_marker, (vec![Type::Bool, type_var("t")], type_var("t"))),
        ("add", add, (vec![Type::Int, Type::Int], Type::Int)),
        ("sub", sub, (vec![Type::Int, Type::Int], Type::Int)),
        ("mget", get, (vec![map_type(), type_var("k")], type_var("v"))),
        ("lget", get, (vec![list_type("t"), Type::Int], type_var("t"))),
        ("t0", t0, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("g"))),
        ("t1", t1, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("h"))),
        ("t2", t2, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("i"))),
        ("t3", t3, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("j"))),
        ("t4", t4, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("k"))),
        ("t5", t5, (vec![tuple_type(&["g", "h", "i", "j", "k", "l"])], type_var("l"))),
        ("insert", insert, (vec![map_type(), type_var("k"), type_var("v")], map_type())),
        ("keys", keys, (vec![map_type()], list_type("k"))),
        ("msize", size, (vec![map_type()], Type::Int)),
        ("lsize", size, (vec![list_type("t")], Type::Int)),
        ("push", push, (vec![list_type("t"), type_var("t")], list_type("t"))),
        ("print", print_pfn, (vec![type_var("t")], Type::Nil)),
        ("eq", eq_pfn, (vec![type_var("t"), type_var("t")], Type::Bool)),
        ("gt", gt_pfn, (vec![type_var("t"), type_var("t")], Type::Bool)),
        ("ge", ge_pfn, (vec![type_var("t"), type_var("t")], Type::Bool)),
        ("lt", lt_pfn, (vec![type_var("t"), type_var("t")], Type::Bool)),
        ("le", le_pfn, (vec![type_var("t"), type_var("t")], Type::Bool)),
    ];
    for (symbol, func, types) in primitives {
        try!(vscope.insert_local(symbol.to_string(),
                                 Rc::new(Value::PrimitiveFn(box (symbol.to_string(), func)))));
        try!(tscope.insert(symbol.to_string(), Type::Fn(box types)));
    }
    Ok(())
}
