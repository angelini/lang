use ast::{Expression, Value};
use primitives;
use rand::{self, Rng};
use scope::ValueScope;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use std::result;

#[derive(Debug)]
pub enum Error {
    CallNonFn(String),
    InvalidPredicate(Value),
    InvalidBlock(Expression),
    UndefinedSymbol(String),
    WrongNumberOfArgs(Vec<String>, Vec<Expression>),
}

pub type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::CallNonFn(ref val) => write!(f, "Called a non fn {:?}", val),
            Error::InvalidPredicate(ref val) => write!(f, "Invalid predicate {:?}", val),
            Error::InvalidBlock(ref expr) => write!(f, "Invalid block {:?}", expr),
            Error::UndefinedSymbol(ref sym) => write!(f, "Undefined symbol {}", sym),
            Error::WrongNumberOfArgs(ref args, ref actual) => {
                write!(f, "Wrong number of args {:?} {:?}", args, actual)
            }
        }
    }
}

fn call_fn(scope: &mut ValueScope,
           fn_key: &str,
           args: Vec<(&str, Expression)>,
           block: &[Expression])
           -> Result<Rc<Value>> {
    let args = args.into_iter()
        .map(|(name, expr)| (name, eval(scope, expr)))
        .collect::<Vec<(&str, Result<Rc<Value>>)>>();

    let scope_id = scope.descend_from(fn_key);
    for (name, arg) in args {
        scope.insert(name.to_string(), try!(arg));
    }

    let last = block.len() - 1;
    for (i, expr) in block.iter().enumerate() {
        if i == last {
            let result = eval(scope, expr.clone());
            scope.ascend();
            scope.jump_to(scope_id);
            return result;
        } else {
            try!(eval(scope, expr.clone()));
        }
    }
    scope.ascend();
    scope.jump_to(scope_id);
    Ok(Rc::new(Value::Nil))
}

fn if_pfn(scope: &mut ValueScope, args: Vec<Expression>) -> Result<Rc<Value>> {
    if args.len() != 3 {
        return Err(Error::WrongNumberOfArgs(vec!["predicate".to_string(),
                                                 "if".to_string(),
                                                 "else".to_string()],
                                            args));
    }

    let mut args = args;
    let r = args.pop().unwrap();
    let l = args.pop().unwrap();

    let pred_expr = args.pop().unwrap();
    let pred = try!(eval(scope, pred_expr));

    match *pred {
        Value::Bool(pred) => if pred { eval(scope, l) } else { eval(scope, r) },
        ref p => Err(Error::InvalidPredicate(p.clone())),
    }
}

fn while_pfn(scope: &mut ValueScope, mut args: Vec<Expression>) -> Result<Rc<Value>> {
    if args.len() != 2 {
        return Err(Error::WrongNumberOfArgs(vec!["predicate".to_string(), "block".to_string()],
                                            args));
    }

    let exprs = match args.pop().unwrap() {
        Expression::Block(exprs) => exprs,
        v => return Err(Error::InvalidBlock(v)),
    };

    let pred = args.pop().unwrap();
    let mut pred_bool = match *try!(eval(scope, pred.clone())) {
        Value::Bool(b) => b,
        ref v => return Err(Error::InvalidPredicate(v.clone())),
    };

    let mut result = Rc::new(Value::Nil);

    while pred_bool {
        scope.descend();
        for expr in exprs.iter().cloned() {
            result = try!(eval(scope, expr.clone()))
        }
        scope.ascend();

        pred_bool = match *try!(eval(scope, pred.clone())) {
            Value::Bool(b) => b,
            ref v => return Err(Error::InvalidPredicate(v.clone())),
        };
    }

    Ok(result)
}

fn call_primitive_fn(scope: &mut ValueScope,
                     args: Vec<Expression>,
                     func: fn(Vec<Rc<Value>>) -> Value)
                     -> Result<Rc<Value>> {
    if func == primitives::if_pfn_marker {
        return if_pfn(scope, args);
    }

    if func == primitives::while_pfn_marker {
        return while_pfn(scope, args);
    }

    let args = args.into_iter()
        .map(|expr| eval(scope, expr))
        .collect::<Result<Vec<Rc<Value>>>>();
    Ok(Rc::new(func(try!(args))))
}

pub fn eval(scope: &mut ValueScope, expr: Expression) -> Result<Rc<Value>> {
    match expr {
        Expression::Assign(box (ref sym, _, ref e)) => {
            let result = try!(eval(scope, e.clone()));
            scope.insert(sym.clone(), result.clone());
            Ok(result)
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
                    try!(eval(scope, expr.clone()));
                }
            }
            scope.ascend();
            Ok(Rc::new(Value::Nil))
        }
        Expression::Call(sym, arg_exprs) => {
            let fn_val = match scope.get(&sym) {
                Some(val) => val.clone(),
                None => return Err(Error::UndefinedSymbol(sym)),
            };

            match *fn_val {
                Value::Fn(box (ref fn_key, ref arg_names, ref block)) => {
                    if arg_names.len() != arg_exprs.len() {
                        let names = arg_names.iter().map(|&(ref n, _)| n.to_string()).collect();
                        return Err(Error::WrongNumberOfArgs(names, arg_exprs));
                    }

                    let args = arg_names.iter()
                        .map(|t| t.0.as_str())
                        .zip(arg_exprs.into_iter())
                        .collect();
                    call_fn(scope, fn_key, args, block)
                }
                Value::PrimitiveFn(box (_, func)) => call_primitive_fn(scope, arg_exprs, func),
                _ => Err(Error::CallNonFn(sym)),
            }
        }
        Expression::List(exprs) => {
            let mut list = Vec::new();
            for expr in exprs.into_iter() {
                list.push(try!(eval(scope, expr)))
            }
            Ok(Rc::new(Value::Vec(list)))
        }
        Expression::Map(pairs) => {
            let mut map = BTreeMap::new();
            for (l, r) in pairs.into_iter() {
                map.insert(try!(eval(scope, l)), try!(eval(scope, r)));
            }
            Ok(Rc::new(Value::Map(map)))
        }
        Expression::Fn(args, exprs) => {
            let num = rand::thread_rng().gen_range(10000, 99999);
            let fn_key = format!("fn_{}", num);
            scope.tag_self_and_parents(&fn_key);
            Ok(Rc::new(Value::Fn(box (fn_key, args, exprs))))
        }
        Expression::Symbol(sym) => {
            if scope.contains_key(&sym) {
                Ok(scope.get(&sym).unwrap().clone())
            } else {
                Err(Error::UndefinedSymbol(sym))
            }
        }
        Expression::Value(val) => Ok(Rc::new(val)),
    }
}
