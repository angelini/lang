#![feature(advanced_slice_patterns, box_patterns, plugin, slice_patterns)]
#![plugin(peg_syntax_ext)]

extern crate rustyline;

mod ast;
mod primitives;
mod scope;

use ast::{Expression, Value};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scope::Scope;
use std::env;
use std::fs::File;
use std::mem;
use std::io::prelude::*;

peg_file! grammar("grammar.rustpeg");

#[derive(Debug)]
enum EvalResult {
    Val(Value),
    Key(String),
}

fn get_or_clone(scope: &Scope, res: EvalResult) -> Value {
    match res {
        EvalResult::Val(v) => v,
        EvalResult::Key(k) => scope.get(&k).unwrap().clone(),
    }
}

fn call_fn(scope: &mut Scope,
           args: Vec<(&String, Expression)>,
           block: &[Expression])
           -> EvalResult {
    let args = args.into_iter()
        .map(|(name, expr)| {
            let result = eval(scope, expr);
            (name, get_or_clone(scope, result))
        })
        .collect::<Vec<(&String, Value)>>();

    scope.descend();

    for (name, arg) in args {
        scope.insert(name.to_string(), arg);
    }

    let last = block.len() - 1;
    for (i, expr) in block.into_iter().enumerate() {
        if i == last {
            let result = eval(scope, expr.clone());
            scope.ascend();
            return result;
        } else {
            eval(scope, expr.clone());
        }
    }
    EvalResult::Val(Value::Nil)
}

fn if_pfn(scope: &mut Scope, args: Vec<Expression>) -> EvalResult {
    assert!(args.len() == 3, "Invalid args to if: {:?}", args);
    let mut args = args;
    let r = args.pop().unwrap();
    let l = args.pop().unwrap();

    let pred_expr = args.pop().unwrap();
    let pred = eval_value(scope, pred_expr);

    match pred {
        Value::Bool(pred) => if pred { eval(scope, l) } else { eval(scope, r) },
        _ => panic!("Invalid predicate to if"),
    }
}

fn while_pfn(scope: &mut Scope, mut args: Vec<Expression>) -> EvalResult {
    assert!(args.len() == 2, "Invalid args to while: {:?}", args);
    let exprs = match args.pop().unwrap() {
        Expression::Block(exprs) => exprs,
        _ => panic!("Invalid block to while"),
    };

    let pred = args.pop().unwrap();
    let mut pred_bool = match eval_value(scope, pred.clone()) {
        Value::Bool(b) => b,
        _ => panic!("Invalid predicate to while"),
    };

    let mut result = EvalResult::Val(Value::Nil);

    while pred_bool {
        scope.descend();
        for expr in exprs.iter().cloned() {
            result = eval(scope, expr.clone())
        }
        scope.ascend();

        pred_bool = match eval_value(scope, pred.clone()) {
            Value::Bool(b) => b,
            _ => panic!("Invalid predicate to while"),
        };
    }

    result
}

fn call_primitive_fn(scope: &mut Scope,
                     args: Vec<Expression>,
                     func: fn(Vec<Value>) -> Value)
                     -> EvalResult {

    if func == primitives::if_pfn_marker {
        return if_pfn(scope, args);
    }

    if func == primitives::while_pfn_marker {
        return while_pfn(scope, args);
    }

    let args = args.into_iter()
        .map(|expr| eval_value(scope, expr))
        .collect::<Vec<Value>>();
    EvalResult::Val(func(args))
}

fn eval(scope: &mut Scope, expr: Expression) -> EvalResult {
    match expr {
        Expression::Assign(sym, e) => {
            let result = eval(scope, *e);
            let val = get_or_clone(scope, result);
            scope.insert(sym.clone(), val);
            EvalResult::Key(sym)
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
            EvalResult::Val(Value::Nil)
        }
        Expression::Call(sym, arg_exprs) => {
            let fn_val = match scope.get(&sym) {
                Some(val) => val.clone(),
                None => panic!("Undefined symbol: {:?}", sym),
            };

            match fn_val {
                Value::Fn(box (ref arg_names, ref block)) => {
                    assert!(arg_names.len() == arg_exprs.len(),
                            "Wrong number of args supplied");
                    let args = arg_names.iter()
                        .zip(arg_exprs.into_iter())
                        .collect();
                    call_fn(scope, args, block)
                }
                Value::PrimitiveFn(func) => call_primitive_fn(scope, arg_exprs, func),
                _ => panic!("Tried to call a non-fn: {:?}", sym),
            }
        }
        Expression::List(items) => {
            let mut val = Vec::new();
            for item in items.into_iter() {
                let result = eval(scope, item);
                val.push(get_or_clone(scope, result))
            }
            EvalResult::Val(Value::Vec(val))
        }
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                EvalResult::Key(sym)
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
        Expression::Value(val) => EvalResult::Val(val),
    }
}

fn eval_value(scope: &mut Scope, expr: Expression) -> Value {
    let result = eval(scope, expr);
    get_or_clone(scope, result)
}

fn parse_and_eval(scope: &mut Scope, line: &str, show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    match grammar::expression(&line) {
        Ok(e) => {
            println!("ret: {:?}", eval_value(scope, e));
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
        _ => start_repl(&mut scope)
    }

}
