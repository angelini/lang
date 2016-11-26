#![feature(advanced_slice_patterns, box_patterns, plugin, slice_patterns)]
#![plugin(peg_syntax_ext)]

extern crate rand;
extern crate rustyline;

mod ast;
mod primitives;
mod scope;

use ast::{Expression, Type, Value};
use rand::Rng;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scope::{TypeScope, ValueScope};
use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::mem;
use std::io::prelude::*;
use std::rc::Rc;

peg_file! grammar("grammar.rustpeg");

fn call_fn(scope: &mut ValueScope,
           fn_key: &str,
           args: Vec<(&str, Expression)>,
           block: &[Expression])
           -> Rc<Value> {
    let args = args.into_iter()
        .map(|(name, expr)| (name, eval(scope, expr)))
        .collect::<Vec<(&str, Rc<Value>)>>();

    let scope_id = scope.descend_from(fn_key);
    for (name, arg) in args {
        scope.insert(name.to_string(), arg);
    }

    let last = block.len() - 1;
    for (i, expr) in block.iter().enumerate() {
        if i == last {
            let result = eval(scope, expr.clone());
            scope.ascend();
            scope.jump_to(scope_id);
            return result;
        } else {
            eval(scope, expr.clone());
        }
    }
    scope.ascend();
    scope.jump_to(scope_id);
    Rc::new(Value::Nil)
}

fn if_pfn(scope: &mut ValueScope, args: Vec<Expression>) -> Rc<Value> {
    assert!(args.len() == 3, "Invalid args to if: {:?}", args);
    let mut args = args;
    let r = args.pop().unwrap();
    let l = args.pop().unwrap();

    let pred_expr = args.pop().unwrap();
    let pred = eval(scope, pred_expr);

    match *pred {
        Value::Bool(pred) => if pred { eval(scope, l) } else { eval(scope, r) },
        _ => panic!("Invalid predicate to if"),
    }
}

fn while_pfn(scope: &mut ValueScope, mut args: Vec<Expression>) -> Rc<Value> {
    assert!(args.len() == 2, "Invalid args to while: {:?}", args);
    let exprs = match args.pop().unwrap() {
        Expression::Block(exprs) => exprs,
        _ => panic!("Invalid block to while"),
    };

    let pred = args.pop().unwrap();
    let mut pred_bool = match *eval(scope, pred.clone()) {
        Value::Bool(b) => b,
        _ => panic!("Invalid predicate to while"),
    };

    let mut result = Rc::new(Value::Nil);

    while pred_bool {
        scope.descend();
        for expr in exprs.iter().cloned() {
            result = eval(scope, expr.clone())
        }
        scope.ascend();

        pred_bool = match *eval(scope, pred.clone()) {
            Value::Bool(b) => b,
            _ => panic!("Invalid predicate to while"),
        };
    }

    result
}

fn call_primitive_fn(scope: &mut ValueScope,
                     args: Vec<Expression>,
                     func: fn(Vec<Rc<Value>>) -> Value)
                     -> Rc<Value> {
    if func == primitives::if_pfn_marker {
        return if_pfn(scope, args);
    }

    if func == primitives::while_pfn_marker {
        return while_pfn(scope, args);
    }

    let args = args.into_iter()
        .map(|expr| eval(scope, expr))
        .collect::<Vec<Rc<Value>>>();
    Rc::new(func(args))
}

fn eval(scope: &mut ValueScope, expr: Expression) -> Rc<Value> {
    match expr {
        Expression::Assign(sym, e) => {
            let result = eval(scope, *e);
            scope.insert(sym.clone(), result.clone());
            result
        }
        Expression::Block(exprs) => {
            scope.descend();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.into_iter().enumerate() {
                if i == last {
                    let result = eval(scope, expr.clone());
                    scope.ascend();
                    return result;
                } else {
                    eval(scope, expr.clone());
                }
            }
            scope.ascend();
            Rc::new(Value::Nil)
        }
        Expression::Call(sym, arg_exprs) => {
            let fn_val = match scope.get(&sym) {
                Some(val) => val.clone(),
                None => panic!("Undefined symbol: {:?}", sym),
            };

            match *fn_val {
                Value::Fn(box (ref fn_key, ref arg_names, ref block)) => {
                    assert!(arg_names.len() == arg_exprs.len(),
                            "Wrong number of args supplied");
                    let args = arg_names.iter()
                        .map(|t| t.0.as_str())
                        .zip(arg_exprs.into_iter())
                        .collect();
                    call_fn(scope, fn_key, args, block)
                }
                Value::PrimitiveFn(func) => call_primitive_fn(scope, arg_exprs, func),
                _ => panic!("Tried to call a non-fn: {:?}", sym),
            }
        }
        Expression::List(exprs) => {
            let mut list = Vec::new();
            for expr in exprs.into_iter() {
                list.push(eval(scope, expr))
            }
            Rc::new(Value::Vec(list))
        }
        Expression::Map(pairs) => {
            let mut map = BTreeMap::new();
            for (l, r) in pairs.into_iter() {
                map.insert(eval(scope, l), eval(scope, r));
            }
            Rc::new(Value::Map(map))
        }
        Expression::Fn(args, exprs) => {
            let num = rand::thread_rng().gen_range(10000, 99999);
            let fn_key = format!("fn_{}", num);
            scope.tag_self_and_parents(&fn_key);
            Rc::new(Value::Fn(Box::new((fn_key, args, exprs))))
        }
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                scope.get(&sym).unwrap().clone()
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
        Expression::Value(val) => Rc::new(val),
    }
}

fn fn_type(scope: &mut TypeScope, args: &[(String, Type)], exprs: &[Expression]) -> (Vec<Type>, Type) {
    let mut arg_types = vec![];
    scope.descend();

    for &(ref name, ref typ) in args {
        scope.insert(name.clone(), typ.clone());
        arg_types.push(typ.clone());
    }

    let last = exprs.len() - 1;
    for (i, expr) in exprs.into_iter().enumerate() {
        if i == last {
            let result = type_check(scope, expr.clone());
            scope.ascend();
            return (arg_types, result)
        } else {
            type_check(scope, expr.clone());
        }
    }
    scope.ascend();
    (arg_types, Type::Nil)
}

fn value_to_type(scope: &mut TypeScope, val: &Value) -> Type {
    match *val {
        Value::Nil => Type::Nil,
        Value::Bool(_) => Type::Bool,
        Value::Int(_) => Type::Int,
        Value::Str(_) => Type::Str,
        Value::Vec(ref values) => {
            if values.is_empty() {
                return Type::Nil;
            }

            let mut typ = None;

            for val in values.iter() {
                typ = match typ {
                    Some(typ) => {
                        let result = value_to_type(scope, val);
                        if result != typ {
                            panic!("Type error, expected: {:?} got: {:?}", typ, result)
                        }
                        Some(typ)
                    }
                    None => Some(value_to_type(scope, val)),
                }
            }

            match typ {
                Some(typ) => typ,
                None => Type::Nil,
            }
        }
        Value::Map(ref pairs) => {
            if pairs.is_empty() {
                return Type::Nil;
            }

            let mut key_typ = None;
            let mut val_typ = None;

            for (key, val) in pairs.iter() {
                key_typ = match key_typ {
                    Some(typ) => {
                        let result = value_to_type(scope, key);
                        if result != typ {
                            panic!("Type error, expected: {:?} got: {:?}", typ, result)
                        }
                        Some(typ)
                    }
                    None => Some(value_to_type(scope, val)),
                };
                val_typ = match val_typ {
                    Some(typ) => {
                        let result = value_to_type(scope, val);
                        if result != typ {
                            panic!("Type error, expected: {:?} got: {:?}", typ, result)
                        }
                        Some(typ)
                    }
                    None => Some(value_to_type(scope, val)),
                }

            }

            match (key_typ, val_typ) {
                (None, None) => Type::Map(Box::new((Type::Nil, Type::Nil))),
                (Some(k), Some(v)) => Type::Map(Box::new((k, v))),
                _ => unreachable!(),
            }
        }
        Value::Fn(box (_, ref args, ref exprs)) => {
            Type::Fn(Box::new(fn_type(scope, args, exprs)))
        }
        Value::PrimitiveFn(_) => Type::Nil,
    }
}

fn type_check(scope: &mut TypeScope, expr: Expression) -> Type {
    match expr {
        Expression::Assign(sym, e) => {
            let result = type_check(scope, *e);
            scope.insert(sym.clone(), result.clone());
            result
        }
        Expression::Block(exprs) => {
            scope.descend();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.into_iter().enumerate() {
                if i == last {
                    let result = type_check(scope, expr.clone());
                    scope.ascend();
                    return result;
                } else {
                    type_check(scope, expr.clone());
                }
            }
            scope.ascend();
            Type::Nil
        }
        Expression::Call(sym, arg_exprs) => {
            let fn_type = match scope.get(&sym) {
                Some(typ) => typ,
                None => panic!("Undefined symbol: {:?}", sym),
            };

            match fn_type {
                Type::Fn(box (ref arg_types, ref ret_type)) => {
                    for (expr, typ) in arg_exprs.into_iter().zip(arg_types.into_iter()) {
                        let result = type_check(scope, expr);
                        if &result != typ {
                            panic!("Type error, expected: {:?} got: {:?}", typ, result)
                        }
                    }
                    ret_type.clone()
                }
                _ => panic!("Tried to call a non-fn: {:?}", sym),
            }
        }
        Expression::List(mut exprs) => {
            if exprs.is_empty() {
                return Type::Nil;
            }

            let last = exprs.pop().unwrap();
            let typ = type_check(scope, last);

            for expr in exprs.into_iter() {
                let result = type_check(scope, expr);
                if result != typ {
                    panic!("Type error, expected: {:?} got: {:?}", typ, result)
                }
            }
            Type::Vec(Box::new(typ))
        }
        Expression::Map(mut pairs) => {
            if pairs.is_empty() {
                return Type::Nil;
            }

            let (key, val) = pairs.pop().unwrap();
            let (key_typ, val_typ) = (type_check(scope, key), type_check(scope, val));

            for (key, val) in pairs.into_iter() {
                let actual_key_typ = type_check(scope, key);
                if actual_key_typ != key_typ {
                    panic!("Type error, expected: {:?} got: {:?}",
                           key_typ,
                           actual_key_typ)
                }
                let actual_val_typ = type_check(scope, val);
                if actual_val_typ != val_typ {
                    panic!("Type error, expected: {:?} got: {:?}",
                           key_typ,
                           actual_val_typ)
                }
            }
            Type::Map(Box::new((key_typ, val_typ)))
        }
        Expression::Fn(ref args, ref exprs) => {
            Type::Fn(Box::new(fn_type(scope, args, exprs)))
        }
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                scope.get(&sym).unwrap()
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
        Expression::Value(val) => value_to_type(scope, &val),
    }
}

fn parse_and_eval(mut tscope: &mut TypeScope,
                  mut vscope: &mut ValueScope,
                  line: &str,
                  show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    match grammar::expression(&line) {
        Ok(e) => {
            println!("typ: {:?}", type_check(tscope, e.clone()));
            println!("ret: {:?}", eval(vscope, e));
            println!("scp: {:?}", vscope);
        }
        Err(err) => println!("parse error: {:?}", err),
    }
    println!("---")
}

fn eval_file(tscope: &mut TypeScope,
             vscope: &mut ValueScope,
             file: &str)
             -> Result<(), std::io::Error> {
    let mut f = try!(File::open(file));
    let mut contents = String::new();
    try!(f.read_to_string(&mut contents));

    match grammar::expressions(&contents.trim()) {
        Ok(exprs) => {
            for expr in exprs {
                eval(vscope, expr);
            }
        }
        Err(err) => panic!("parse error: {:?}", err),
    }
    Ok(())
}

const HISTORY_FILE: &'static str = "history.txt";

fn start_repl(mut tscope: &mut TypeScope, mut vscope: &mut ValueScope) {
    parse_and_eval(&mut tscope, &mut vscope, "foo = 1", true);
    parse_and_eval(&mut tscope, &mut vscope, "bar = [1, 2, foo]", true);
    parse_and_eval(&mut tscope, &mut vscope, "identity = fn (id: int) { id }", true);

    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history(HISTORY_FILE) {
        println!("No previous history");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                parse_and_eval(&mut tscope, &mut vscope, &line, false);
            }
            Err(ReadlineError::Interrupted) => {
                println!("ctrl-c");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("ctrl-d");
                break;
            }
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(HISTORY_FILE).unwrap();
}

fn main() {
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());

    let args = env::args().collect::<Vec<String>>();

    let mut tscope = TypeScope::new();
    let mut vscope = ValueScope::new();
    primitives::add_primitive_fns(&mut vscope);

    match args[1..] {
        [ref file] => eval_file(&mut tscope, &mut vscope, file).unwrap(),
        _ => start_repl(&mut tscope, &mut vscope),
    }
}
