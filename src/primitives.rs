use ast::Value;
use scope::Scope;

fn add(args: Vec<Value>) -> Value {
    match args[..] {
        [Value::Int(l), Value::Int(r)] => Value::Int(l + r),
        [ref a..] => panic!("Invalid args to add: {:?}", a),
    }
}

macro_rules! cmp_branches {
    ( $n:ident, $e:expr, $op:ident, $( $t:path ),* ) => {{
        match $e[..] {
            $(
                [$t(ref l), $t(ref r)] => Value::Bool(l.$op(r)),
            )*
            [ref a..] => panic!("Invalid args to $n: {:?}", a),
        }
    }};
}

fn eq_pfn(args: Vec<Value>) -> Value {
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

fn gt_pfn(args: Vec<Value>) -> Value {
    cmp_branches!(greater_than, args, gt, Value::Int, Value::Str, Value::Vec)
}

fn ge_pfn(args: Vec<Value>) -> Value {
    cmp_branches!(greater_equal, args, ge, Value::Int, Value::Str, Value::Vec)
}

fn lt_pfn(args: Vec<Value>) -> Value {
    cmp_branches!(less_than, args, lt, Value::Int, Value::Str, Value::Vec)
}

fn le_pfn(args: Vec<Value>) -> Value {
    cmp_branches!(less_equal, args, le, Value::Int, Value::Str, Value::Vec)
}

fn lget(mut args: Vec<Value>) -> Value {
    match args[..] {
        [Value::Vec(ref mut l), Value::Int(i)] => {
            match l.get(i as usize) {
                Some(v) => v.clone(),
                None => Value::Nil,
            }
        }
        [ref a..] => panic!("Invalid args to lget: {:?}", a),
    }
}

fn lpush(mut args: Vec<Value>) -> Value {
    assert!(args.len() == 2, "Invalid args to lpush: {:?}", args);
    let val = args.pop().unwrap();
    let list = args.pop().unwrap();

    match (list, val) {
        (Value::Vec(mut l), v) => {
            l.push(v);
            Value::Vec(l)
        }
        (l, v) => panic!("Invalid args to lpush: {:?} {:?}", l, v),
    }
}

fn print_pfn(args: Vec<Value>) -> Value {
    println!("stdout >> {:?}", args);
    Value::Nil
}

pub fn if_pfn_marker(_: Vec<Value>) -> Value {
    unreachable!()
}

pub fn while_pfn_marker(_: Vec<Value>) -> Value {
    unreachable!()
}

pub fn add_primitive_fns(scope: &mut Scope) {
    scope.insert("if".to_string(),
                 Value::PrimitiveFn(if_pfn_marker));
    scope.insert("while".to_string(),
                 Value::PrimitiveFn(while_pfn_marker));

    scope.insert("add".to_string(), Value::PrimitiveFn(add));
    scope.insert("lget".to_string(), Value::PrimitiveFn(lget));
    scope.insert("lpush".to_string(), Value::PrimitiveFn(lpush));
    scope.insert("print".to_string(), Value::PrimitiveFn(print_pfn));

    scope.insert("eq".to_string(), Value::PrimitiveFn(eq_pfn));
    scope.insert("gt".to_string(), Value::PrimitiveFn(gt_pfn));
    scope.insert("ge".to_string(), Value::PrimitiveFn(ge_pfn));
    scope.insert("lt".to_string(), Value::PrimitiveFn(lt_pfn));
    scope.insert("le".to_string(), Value::PrimitiveFn(le_pfn));
}
