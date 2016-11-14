#![feature(box_patterns, plugin)]
#![plugin(peg_syntax_ext)]

extern crate rustyline;

mod ast;

use ast::{Expression, Value};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::mem;

peg_file! grammar("grammar.rustpeg");

#[derive(Clone, Debug)]
pub struct Scope {
    levels: Vec<HashMap<String, Value>>,
}

impl Scope {
    fn new() -> Scope {
        Scope { levels: vec![HashMap::new()] }
    }

    fn descend(&mut self) {
        self.levels.push(HashMap::new())
    }

    fn ascend(&mut self) {
        self.levels.pop();
    }

    fn insert(&mut self, key: String, val: Value) -> Option<Value> {
        let last = self.levels.len() - 1;
        self.levels.get_mut(last).unwrap().insert(key, val)
    }

    fn get(&self, key: &str) -> Option<&Value> {
        for level in self.levels.iter().rev() {
            if level.contains_key(key) {
                return level.get(key);
            }
        }
        None
    }

    fn contains_key(&self, key: &str) -> bool {
        for level in self.levels.iter().rev() {
            if level.contains_key(key) {
                return true;
            }
        }
        false
    }
}

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
            let val = get_or_clone(scope, result);
            scope.ascend();
            return EvalResult::Val(val);
        } else {
            eval(scope, expr.clone());
        }
    }
    panic!("Call to empty fn")
}

fn eval(scope: &mut Scope, expr: Expression) -> EvalResult {
    match expr {
        Expression::Call(sym, arg_exprs) => {
            let fn_val = match scope.get(&sym) {
                Some(val) => val.clone(),
                None => panic!("Undefined symbol: {:?}", sym),
            };

            match fn_val {
                Value::Fn(box (ref arg_names, ref block)) => {
                    assert!(arg_names.len() == arg_exprs.len(),
                            "Wrong number of args supplied");
                    let args = arg_names.iter().zip(arg_exprs.into_iter()).collect();
                    call_fn(scope, args, block)
                }
                Value::PrimitiveFn(box func) => {
                    let args = arg_exprs.into_iter()
                        .map(|expr| {
                            let result = eval(scope, expr);
                            get_or_clone(scope, result)
                        })
                        .collect::<Vec<Value>>();
                    EvalResult::Val(func(args))
                }
                _ => panic!("Tried to call a non-fn: {:?}", sym),
            }
        }
        Expression::Assign(sym, e) => {
            let result = eval(scope, *e);
            let val = get_or_clone(scope, result);
            scope.insert(sym.clone(), val);
            EvalResult::Key(sym)
        }
        Expression::List(items) => {
            let mut val = Vec::new();
            for item in items.into_iter() {
                let result = eval(scope, item);
                val.push(get_or_clone(scope, result))
            }
            EvalResult::Val(Value::Vec(val))
        }
        Expression::Value(val) => EvalResult::Val(val),
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                EvalResult::Key(sym)
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
    }
}

fn parse_and_eval(scope: &mut Scope, line: &str) {
    println!("lin: {:?}", line);
    match grammar::expression(&line) {
        Ok(e) => {
            println!("ret: {:?}", eval(scope, e));
            println!("scp: {:?}", scope);
        }
        Err(err) => println!("parse error: {:?}", err),
    }
    println!("---")
}

fn add(args: Vec<Value>) -> Value {
    match (&args[0], &args[1]) {
        (&Value::Int(l), &Value::Int(r)) => Value::Int(l + r),
        _ => panic!("Invalid args to add: {:?}", args),
    }
}

macro_rules! equal_branches {
    ( $e:expr, $( $x:path ),* ) => {{
        assert!($e.len() == 2, "Invalid args to equal: {:?}", $e);
        let mut args = $e;
        let r = args.pop().unwrap();
        let l = args.pop().unwrap();
        match (l, r) {
            $(
                ($x(l), $x(r)) => Value::Bool(l == r),
            )*
            _ => panic!("Invalid args to equal: {:?}", args),
        }
    }};
}

fn equal(args: Vec<Value>) -> Value {
    equal_branches!(args, Value::Bool, Value::Int, Value::Str, Value::Vec, Value::Fn, Value::PrimitiveFn)
}

macro_rules! if_branches {
    ( $e:expr, $( $x:path ),* ) => {{
        assert!($e.len() == 3, "Invalid args to if: {:?}", $e);
        let mut args = $e;
        let r = args.pop().unwrap();
        let l = args.pop().unwrap();
        let pred = args.pop().unwrap();
        match (pred, l, r) {
            $(
                (Value::Bool(pred), $x(l), $x(r)) => {
                    if pred {
                        $x(l)
                    } else {
                        $x(r)
                    }
                }
            )*
            _ => panic!("Invalid args to if: {:?}", args),
        }
    }};
}

// NOTE: both while and if require un-evaluated expressions

fn if_pfn(args: Vec<Value>) -> Value {
    // FIXME: else branch is evaluated even if arg[0] is true
    if_branches!(args, Value::Bool, Value::Int, Value::Str, Value::Vec)
}

fn lget(args: Vec<Value>) -> Value {
    let mut args = args;
    let r = args.pop().unwrap();
    let l = args.pop().unwrap();
    match (l, r) {
        (Value::Vec(ref mut l), Value::Int(i)) => l.remove(i as usize),
        _ => panic!("Invalid args to lget: {:?}", args),
    }
}

fn main() {
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());

    let mut scope = Scope::new();

    scope.insert("add".to_string(), Value::PrimitiveFn(Box::new(add)));
    scope.insert("equal".to_string(), Value::PrimitiveFn(Box::new(equal)));
    scope.insert("lget".to_string(), Value::PrimitiveFn(Box::new(lget)));
    scope.insert("if".to_string(), Value::PrimitiveFn(Box::new(if_pfn)));

    parse_and_eval(&mut scope, "foo = 1");
    parse_and_eval(&mut scope, "bar = [1, 2, foo]");
    parse_and_eval(&mut scope, "identity = fn (id) { id }");

    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history("history.txt") {
        println!("No previous history");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                parse_and_eval(&mut scope, &line);
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
    rl.save_history("history.txt").unwrap();
}
