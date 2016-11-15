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

    fn insert(&mut self, key: String, val: Value) {
        for level in self.levels.iter_mut().rev() {
            if level.contains_key(&key) {
                level.insert(key, val);
                return
            }
        }
        let last = self.levels.len() - 1;
        self.levels.get_mut(last).unwrap().insert(key, val);
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

fn if_pfn_marker(_: Vec<Value>) -> Value {
    unreachable!()
}

fn while_pfn(scope: &mut Scope, args: Vec<Expression>) -> EvalResult {
    assert!(args.len() == 2, "Invalid args to while: {:?}", args);
    let mut args = args;
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

fn while_pfn_marker(_: Vec<Value>) -> Value {
    unreachable!()
}

fn call_primitive_fn(scope: &mut Scope,
                     args: Vec<Expression>,
                     func: fn(Vec<Value>) -> Value)
                     -> EvalResult {

    if func == if_pfn_marker {
        return if_pfn(scope, args);
    }

    if func == while_pfn_marker {
        return while_pfn(scope, args);
    }

    let args = args.into_iter()
        .map(|expr| {
            let result = eval(scope, expr);
            get_or_clone(scope, result)
        })
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
                Value::PrimitiveFn(box func) => call_primitive_fn(scope, arg_exprs, func),
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
    equal_branches!(args,
                    Value::Bool,
                    Value::Int,
                    Value::Str,
                    Value::Vec,
                    Value::Fn,
                    Value::PrimitiveFn)
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

fn print_pfn(args: Vec<Value>) -> Value {
    println!("stdout >> {:?}", args);
    Value::Nil
}

fn main() {
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());

    let mut scope = Scope::new();

    scope.insert("add".to_string(), Value::PrimitiveFn(Box::new(add)));
    scope.insert("equal".to_string(), Value::PrimitiveFn(Box::new(equal)));
    scope.insert("if".to_string(),
                 Value::PrimitiveFn(Box::new(if_pfn_marker)));
    scope.insert("lget".to_string(), Value::PrimitiveFn(Box::new(lget)));
    scope.insert("print".to_string(), Value::PrimitiveFn(Box::new(print_pfn)));
    scope.insert("while".to_string(),
                 Value::PrimitiveFn(Box::new(while_pfn_marker)));


    parse_and_eval(&mut scope, "foo = 1", true);
    parse_and_eval(&mut scope, "bar = [1, 2, foo]", true);
    parse_and_eval(&mut scope, "identity = fn (id) { id }", true);

    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history("history.txt") {
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
    rl.save_history("history.txt").unwrap();
}
