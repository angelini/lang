use ast::{Expression, Value};
use primitives;
use rand::{self, Rng};
use scope::ValueScope;
use std::collections::BTreeMap;
use std::rc::Rc;

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

pub fn eval(scope: &mut ValueScope, expr: Expression) -> Rc<Value> {
    match expr {
        Expression::Assign(box (ref sym, _, ref e)) => {
            let result = eval(scope, e.clone());
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
                Value::PrimitiveFn(box (_, func)) => call_primitive_fn(scope, arg_exprs, func),
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
