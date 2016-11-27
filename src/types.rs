use ast::{Expression, Type, Value};
use scope::TypeScope;
use std::collections::HashMap;

fn fn_type(scope: &mut TypeScope,
           args: &[(String, Type)],
           exprs: &[&Expression])
           -> (Vec<Type>, Type) {
    let mut arg_types = vec![];
    scope.descend();

    for &(ref name, ref typ) in args {
        scope.insert(name.clone(), typ.clone());
        arg_types.push(typ.clone());
    }

    let last = exprs.len() - 1;
    for (i, expr) in exprs.iter().enumerate() {
        if i == last {
            let result = type_check(scope, expr);
            scope.ascend();
            return (arg_types, result);
        } else {
            type_check(scope, expr);
        }
    }
    scope.ascend();
    (arg_types, Type::Nil)
}

fn primitive_fn_type(scope: &mut TypeScope, symbol: &str) -> (Vec<Type>, Type) {
    match scope.get(symbol) {
        Some(Type::Fn(box (ref arg_types, ref ret_type))) => (arg_types.clone(), ret_type.clone()),
        _ => panic!("Type error: primitive fn not found: {}", symbol),
    }
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
            Type::Fn(Box::new(fn_type(scope, args, &exprs.iter().collect::<Vec<&Expression>>())))
        }
        Value::PrimitiveFn(box (ref symbol, _)) => {
            Type::Fn(Box::new(primitive_fn_type(scope, symbol)))
        }
    }
}

fn bind_type(bindings: &mut HashMap<String, Type>,
             unbound: &Type,
             expected: Option<&Type>)
             -> Type {
    match *unbound {
        Type::Unknown => {
            match expected {
                Some(typ) => typ.clone(),
                None => Type::Unknown,
            }
        }
        Type::Var(ref name) => {
            if !bindings.contains_key(name) {
                match expected {
                    Some(typ) => bindings.insert(name.to_string(), typ.clone()),
                    None => bindings.insert(name.to_string(), Type::Var(name.clone())),
                };
            }
            bindings.get(name).unwrap().clone()
        }
        Type::Vec(box ref t) => {
            let expected_t = match expected {
                Some(&Type::Vec(box ref expected_t)) => Some(expected_t),
                None => None,
                _ => {
                    panic!("Binding error, unbound: {:?} expected: {:?}",
                           unbound,
                           expected)
                }
            };
            Type::Vec(Box::new(bind_type(bindings, t, expected_t)))
        }
        Type::Map(box (ref key_t, ref val_t)) => {
            let (expected_key_t, expected_val_t) = match expected {
                Some(&Type::Map(box (ref expected_key_t, ref expected_val_t))) => {
                    (Some(expected_key_t), Some(expected_val_t))
                }
                None => (None, None),
                _ => {
                    panic!("Binding error, unbound: {:?} expected: {:?}",
                           unbound,
                           expected)
                }
            };
            Type::Map(Box::new((bind_type(bindings, key_t, expected_key_t),
                                bind_type(bindings, val_t, expected_val_t))))
        }
        Type::Fn(box (ref arg_types, ref ret_type)) => {
            match expected {
                Some(&Type::Fn(box (ref exp_arg_types, ref exp_ret_type))) => {
                    let bound_args = arg_types.iter()
                        .zip(exp_arg_types.iter())
                        .map(|(typ, expected)| bind_type(bindings, typ, Some(expected)))
                        .collect();
                    Type::Fn(Box::new((bound_args,
                                       bind_type(bindings, ret_type, Some(exp_ret_type)))))
                }
                None => {
                    let bound_args = arg_types.iter()
                        .map(|typ| bind_type(bindings, typ, None))
                        .collect();
                    Type::Fn(Box::new((bound_args, bind_type(bindings, ret_type, None))))
                }
                _ => {
                    panic!("Binding error, unbound: {:?} expected: {:?}",
                           unbound,
                           expected)
                }
            }
        }
        ref unbound => unbound.clone(),
    }
}

pub fn type_check(scope: &mut TypeScope, expr: &Expression) -> Type {
    match *expr {
        Expression::Assign(box (ref sym, ref hint, ref e)) => {
            let expr_type = type_check(scope, e);
            let mut bindings = HashMap::new();

            let result = match *hint {
                Some(ref h) => bind_type(&mut bindings, &expr_type, Some(h)),
                None => bind_type(&mut bindings, &expr_type, None),
            };

            scope.insert(sym.clone(), result.clone());
            result.clone()
        }
        Expression::Block(ref exprs) => {
            scope.descend();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.into_iter().enumerate() {
                if i == last {
                    let result = type_check(scope, &expr);
                    scope.ascend();
                    return result;
                } else {
                    type_check(scope, &expr);
                }
            }
            scope.ascend();
            Type::Nil
        }
        Expression::Call(ref sym, ref arg_exprs) => {
            let fn_type = match scope.get(&sym) {
                Some(typ) => typ,
                None => panic!("Undefined symbol: {:?}", sym),
            };
            match fn_type {
                Type::Fn(box (ref arg_types, ref ret_type)) => {
                    let mut bindings = HashMap::new();

                    for (expr, unbound_type) in arg_exprs.into_iter().zip(arg_types.into_iter()) {
                        let expected = type_check(scope, &expr);
                        let bound = bind_type(&mut bindings, &unbound_type, Some(&expected));
                        if bound != expected {
                            panic!("Type error, expected: {:?} got: {:?}", expected, bound)
                        }
                    }
                    bind_type(&mut bindings, ret_type, None)
                }
                _ => panic!("Tried to call a non-fn: {:?}", sym),
            }
        }
        Expression::List(ref exprs) => {
            if exprs.is_empty() {
                return Type::Vec(Box::new(Type::Unknown));
            }

            let first = exprs.get(0).unwrap();
            let typ = type_check(scope, &first);

            for expr in exprs.iter().skip(1) {
                let result = type_check(scope, &expr);
                if result != typ {
                    panic!("Type error, expected: {:?} got: {:?}", typ, result)
                }
            }
            Type::Vec(Box::new(typ))
        }
        Expression::Map(ref pairs) => {
            if pairs.is_empty() {
                return Type::Map(Box::new((Type::Unknown, Type::Unknown)));
            }

            let &(ref key, ref val) = pairs.get(0).unwrap();
            let (key_typ, val_typ) = (type_check(scope, &key), type_check(scope, &val));

            for &(ref key, ref val) in pairs.iter().skip(1) {
                let actual_key_typ = type_check(scope, &key);
                if actual_key_typ != key_typ {
                    panic!("Type error, expected: {:?} got: {:?}",
                           key_typ,
                           actual_key_typ)
                }
                let actual_val_typ = type_check(scope, &val);
                if actual_val_typ != val_typ {
                    panic!("Type error, expected: {:?} got: {:?}",
                           key_typ,
                           actual_val_typ)
                }
            }
            Type::Map(Box::new((key_typ, val_typ)))
        }
        Expression::Fn(ref args, ref exprs) => {
            Type::Fn(Box::new(fn_type(scope, args, &exprs.iter().collect::<Vec<&Expression>>())))
        }
        Expression::Symbol(ref sym) => {
            if scope.contains_key(sym) {
                scope.get(&sym).unwrap()
            } else {
                panic!("Undefined symbol: {:?}", sym)
            }
        }
        Expression::Value(ref val) => value_to_type(scope, val),
    }
}
