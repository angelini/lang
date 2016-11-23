#![feature(advanced_slice_patterns, box_patterns, plugin, slice_patterns)]
#![plugin(peg_syntax_ext)]

extern crate rand;
extern crate rustyline;

mod ast;
mod primitives;
mod scope;

use ast::{Expression, Value};
use rand::Rng;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scope::Scope;
use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::mem;
use std::io::prelude::*;
use std::rc::Rc;

peg_file! grammar("grammar.rustpeg");

fn call_fn(scope: &mut Scope,
           fn_key: &str,
           args: Vec<(&String, Expression)>,
           block: &[Expression])
           -> Rc<Value> {
    let args = args.into_iter()
        .map(|(name, expr)| (name, eval(scope, expr)))
        .collect::<Vec<(&String, Rc<Value>)>>();

    let scope_id = scope.descend_from(fn_key);
    for (name, arg) in args {
        scope.insert(name.to_string(), arg);
    }

    let last = block.len() - 1;
    for (i, expr) in block.into_iter().enumerate() {
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

fn if_pfn(scope: &mut Scope, args: Vec<Expression>) -> Rc<Value> {
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

fn while_pfn(scope: &mut Scope, mut args: Vec<Expression>) -> Rc<Value> {
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

fn call_primitive_fn(scope: &mut Scope,
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

fn eval(scope: &mut Scope, expr: Expression) -> Rc<Value> {
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
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                scope.get(&sym).unwrap().clone()
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
        Expression::Value(val) => {
            let val = match val {
                Value::RawFn(box (ref args, ref exprs)) => {
                    let num = rand::thread_rng().gen_range(10000, 99999);
                    Value::Fn(Box::new((format!("fn_{}", num), args.clone(), exprs.clone())))
                }
                v => v,
            };
            Rc::new(val)
        }
    }
}

fn parse_and_eval(scope: &mut Scope, line: &str, show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    match grammar::expression(&line) {
        Ok(e) => {
            println!("ret: {:?}", eval(scope, e));
            println!("scp: {:?}", scope);
        }
        Err(err) => println!("parse error: {:?}", err),
    }
    println!("---")
}

fn eval_file(scope: &mut Scope, file: &str) -> Result<(), std::io::Error> {
    let mut f = try!(File::open(file));
    let mut contents = String::new();
    try!(f.read_to_string(&mut contents));

    match grammar::expressions(&contents.trim()) {
        Ok(exprs) => {
            for expr in exprs {
                eval(scope, expr);
            }
        }
        Err(err) => panic!("parse error: {:?}", err),
    }
    Ok(())
}

const HISTORY_FILE: &'static str = "history.txt";

fn start_repl(mut scope: &mut Scope) {
    parse_and_eval(&mut scope, "foo = 1", true);
    parse_and_eval(&mut scope, "bar = [1, 2, foo]", true);
    parse_and_eval(&mut scope, "identity = fn (id) { id }", true);

    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history(HISTORY_FILE) {
        println!("No previous history");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                parse_and_eval(&mut scope, &line, false);
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

    let mut scope = Scope::new();
    primitives::add_primitive_fns(&mut scope);

    match args[1..] {
        [ref file] => eval_file(&mut scope, file).unwrap(),
        _ => start_repl(&mut scope),
    }
}
