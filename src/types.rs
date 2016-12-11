use ast::{Expression, Type, Value};
use scope::{self, TypeScope};
use std::collections::HashMap;
use std::fmt;
use std::result;

#[derive(Debug)]
pub enum Error {
    ArgLength(usize, usize),
    Binding(Type, Type),
    CallNonFn(Type),
    PrimitiveFnNotFound(String),
    Scope(scope::Error),
    TypeMismatch(Type, Type),
    UndefinedSymbol(String),
}

impl From<scope::Error> for Error {
    fn from(err: scope::Error) -> Error {
        Error::Scope(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::ArgLength(exp, act) => {
                write!(f,
                       "Argument length error, expected: {:?} actual: {:?}",
                       exp,
                       act)
            }
            Error::Binding(ref exp, ref act) => {
                write!(f, "Binding error, expected: {:?} actual: {:?}", exp, act)
            }
            Error::CallNonFn(ref typ) => write!(f, "Called a non fn {:?}", typ),
            Error::PrimitiveFnNotFound(ref name) => write!(f, "Primitive fn not found {}", name),
            Error::Scope(ref err) => write!(f, "{}", err),
            Error::TypeMismatch(ref exp, ref act) => {
                write!(f, "Type mismatch, expected: {:?} actual: {:?}", exp, act)
            }
            Error::UndefinedSymbol(ref sym) => write!(f, "Undefined symbol {}", sym),
        }
    }
}

fn fn_type(scope: &mut TypeScope,
           args: &[(String, Type)],
           exprs: &[&Expression])
           -> Result<(Vec<Type>, Type)> {
    let mut arg_types = vec![];
    scope.descend();

    for &(ref name, ref typ) in args {
        try!(scope.insert(name.clone(), typ.clone()));
        arg_types.push(typ.clone());
    }

    let last = exprs.len() - 1;
    for (i, expr) in exprs.iter().enumerate() {
        if i == last {
            let result = try!(type_check(scope, expr));
            try!(scope.ascend());
            return Ok((arg_types, result));
        } else {
            try!(type_check(scope, expr));
        }
    }
    try!(scope.ascend());
    Ok((arg_types, Type::Nil))
}

fn primitive_fn_type(scope: &mut TypeScope, symbol: &str) -> Result<(Vec<Type>, Type)> {
    match scope.get(symbol) {
        Some(Type::Fn(box (ref arg_types, ref ret_type))) => {
            Ok((arg_types.clone(), ret_type.clone()))
        }
        _ => Err(Error::PrimitiveFnNotFound(symbol.to_string())),
    }
}

fn value_to_type(scope: &mut TypeScope, val: &Value) -> Result<Type> {
    match *val {
        Value::Nil => Ok(Type::Nil),
        Value::Bool(_) => Ok(Type::Bool),
        Value::Int(_) => Ok(Type::Int),
        Value::Str(_) => Ok(Type::Str),
        Value::List(ref values) => {
            let mut expected = None;
            for val in values.iter() {
                expected = match expected {
                    Some(exp) => {
                        let actual = try!(value_to_type(scope, val));
                        if actual != exp {
                            return Err(Error::TypeMismatch(exp, actual));
                        }
                        Some(exp)
                    }
                    None => Some(try!(value_to_type(scope, val))),
                }
            }

            Ok(match expected {
                Some(typ) => typ,
                None => Type::List(box Type::Unknown),
            })
        }
        Value::Tuple(ref values) => {
            let types = values.iter()
                .map(|val| value_to_type(scope, val))
                .collect();
            Ok(Type::Tuple(try!(types)))
        }
        Value::Map(ref pairs) => {
            let mut key_exp = None;
            let mut val_exp = None;

            for (key, val) in pairs.iter() {
                key_exp = match key_exp {
                    Some(exp) => {
                        let actual = try!(value_to_type(scope, key));
                        if actual != exp {
                            return Err(Error::TypeMismatch(exp, actual));
                        }
                        Some(exp)
                    }
                    None => Some(try!(value_to_type(scope, val))),
                };
                val_exp = match val_exp {
                    Some(exp) => {
                        let actual = try!(value_to_type(scope, val));
                        if actual != exp {
                            return Err(Error::TypeMismatch(exp, actual));
                        }
                        Some(exp)
                    }
                    None => Some(try!(value_to_type(scope, val))),
                }

            }

            Ok(match (key_exp, val_exp) {
                (None, None) => Type::Map(box (Type::Unknown, Type::Unknown)),
                (Some(k), Some(v)) => Type::Map(box (k, v)),
                _ => unreachable!(),
            })
        }
        Value::Fn(box (_, ref args, ref exprs)) => {
            Ok(Type::Fn(box try!(fn_type(scope,
                                         args,
                                         &exprs.iter().collect::<Vec<&Expression>>()))))
        }
        Value::PrimitiveFn(box (ref symbol, _)) => {
            Ok(Type::Fn(box try!(primitive_fn_type(scope, symbol))))
        }
    }
}

fn bind_type(bindings: &mut HashMap<String, Type>,
             unbound: &Type,
             expected: Option<&Type>)
             -> Result<Type> {
    match *unbound {
        Type::Unknown => {
            Ok(match expected {
                Some(typ) => typ.clone(),
                None => Type::Unknown,
            })
        }
        Type::Var(ref name) => {
            if !bindings.contains_key(name) {
                match expected {
                    Some(typ) => bindings.insert(name.to_string(), typ.clone()),
                    None => bindings.insert(name.to_string(), Type::Var(name.clone())),
                };
            }

            let typ = bindings.get(name).unwrap().clone();

            if let Type::Var(_) = typ {
                return Ok(typ);
            }

            let bound = try!(bind_type(bindings, &typ, expected));
            if bound != typ {
                bindings.insert(name.to_string(), bound.clone());
            }
            Ok(bound)
        }
        Type::List(box ref t) => {
            let expected_t = match expected {
                Some(&Type::List(box ref expected_t)) => Some(expected_t),
                Some(&Type::Unknown) |
                None => None,
                _ => return Err(Error::Binding(expected.unwrap().clone(), unbound.clone())),
            };
            Ok(Type::List(box try!(bind_type(bindings, t, expected_t))))
        }
        Type::Tuple(ref ts) => {
            match expected {
                Some(&Type::Tuple(ref expected_ts)) => {
                    let bound_ts = ts.iter()
                        .zip(expected_ts.iter())
                        .map(|(typ, expected)| bind_type(bindings, typ, Some(expected)))
                        .collect::<Result<Vec<Type>>>();
                    Ok(Type::Tuple(try!(bound_ts)))
                }
                Some(&Type::Unknown) |
                None => {
                    let bound_ts = ts.iter()
                        .map(|typ| bind_type(bindings, typ, None))
                        .collect::<Result<Vec<Type>>>();
                    Ok(Type::Tuple(try!(bound_ts)))
                }
                _ => Err(Error::Binding(expected.unwrap().clone(), unbound.clone())),
            }
        }
        Type::Map(box (ref key_t, ref val_t)) => {
            let (expected_key_t, expected_val_t) = match expected {
                Some(&Type::Map(box (ref expected_key_t, ref expected_val_t))) => {
                    (Some(expected_key_t), Some(expected_val_t))
                }
                Some(&Type::Unknown) |
                None => (None, None),
                _ => return Err(Error::Binding(expected.unwrap().clone(), unbound.clone())),
            };
            Ok(Type::Map(box (try!(bind_type(bindings, key_t, expected_key_t)),
                              try!(bind_type(bindings, val_t, expected_val_t)))))
        }
        Type::Fn(box (ref arg_types, ref ret_type)) => {
            match expected {
                Some(&Type::Fn(box (ref exp_arg_types, ref exp_ret_type))) => {
                    let bound_args = arg_types.iter()
                        .zip(exp_arg_types.iter())
                        .map(|(typ, expected)| bind_type(bindings, typ, Some(expected)))
                        .collect::<Result<Vec<Type>>>();
                    Ok(Type::Fn(box (try!(bound_args),
                                     try!(bind_type(bindings, ret_type, Some(exp_ret_type))))))
                }
                Some(&Type::Unknown) |
                None => {
                    let bound_args = arg_types.iter()
                        .map(|typ| bind_type(bindings, typ, None))
                        .collect::<Result<Vec<Type>>>();
                    Ok(Type::Fn(box (try!(bound_args), try!(bind_type(bindings, ret_type, None)))))
                }
                _ => Err(Error::Binding(expected.unwrap().clone(), unbound.clone())),
            }
        }
        ref unbound => Ok(unbound.clone()),
    }
}

fn is_more_precise(less: &Type, more: &Type) -> bool {
    if less == more {
        return true;
    }

    match (less, more) {
        (&Type::Unknown, _) |
        (&Type::Var(_), &Type::Var(_)) => true,
        (&Type::List(box ref inner_less), &Type::List(box ref inner_more)) => {
            is_more_precise(inner_less, inner_more)
        }
        (&Type::Tuple(ref less_ts), &Type::Tuple(ref more_ts)) => {
            let mut result = true;
            for (l, m) in less_ts.iter().zip(more_ts.iter()) {
                result = result && is_more_precise(l, m);
            }
            result
        }
        (&Type::Map(box (ref less_key, ref less_val)),
         &Type::Map(box (ref more_key, ref more_val))) => {
            is_more_precise(less_key, more_key) && is_more_precise(less_val, more_val)
        }
        (&Type::Fn(box (ref less_args, ref less_out)),
         &Type::Fn(box (ref more_args, ref more_out))) => {
            let mut result = true;
            for (l, m) in less_args.iter().zip(more_args.iter()) {
                result = result && is_more_precise(l, m);
            }
            result && is_more_precise(less_out, more_out)
        }
        _ => false,
    }
}

pub fn type_check(scope: &mut TypeScope, expr: &Expression) -> Result<Type> {
    match *expr {
        Expression::Assign(box (local, ref sym, ref hint, ref e)) => {
            if local {
                try!(scope.insert(sym.clone(), Type::Unknown));
            }

            let expr_type = try!(type_check(scope, e));
            let mut bindings = HashMap::new();

            let actual = match *hint {
                Some(ref h) => try!(bind_type(&mut bindings, &expr_type, Some(h))),
                None => try!(bind_type(&mut bindings, &expr_type, None)),
            };

            if local {
                try!(scope.insert(sym.clone(), actual.clone()))
            } else {
                match scope.get(sym) {
                    Some(expected) => {
                        if expected != actual {
                            return Err(Error::TypeMismatch(expected, actual));
                        }
                    }
                    None => return Err(Error::UndefinedSymbol(sym.to_string())),
                }

            }
            Ok(actual.clone())
        }
        Expression::Block(ref exprs) => {
            scope.descend();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.into_iter().enumerate() {
                if i == last {
                    let result = type_check(scope, expr);
                    try!(scope.ascend());
                    return result;
                } else {
                    try!(type_check(scope, &expr));
                }
            }
            try!(scope.ascend());
            Ok(Type::Nil)
        }
        Expression::Call(ref sym, ref arg_exprs) => {
            let fn_type = match scope.get(sym) {
                Some(typ) => typ,
                None => return Err(Error::UndefinedSymbol(sym.to_string())),
            };
            match fn_type {
                Type::Fn(box (ref arg_types, ref ret_type)) => {
                    if arg_types.len() != arg_exprs.len() {
                        return Err(Error::ArgLength(arg_types.len(), arg_exprs.len()));
                    }

                    let mut bindings = HashMap::new();

                    for (arg_expr, arg_type) in arg_exprs.into_iter().zip(arg_types.into_iter()) {
                        let arg_expr_type = try!(type_check(scope, &arg_expr));
                        let bound = try!(bind_type(&mut bindings, &arg_type, Some(&arg_expr_type)));
                        if !is_more_precise(&arg_expr_type, &bound) {
                            return Err(Error::TypeMismatch(bound, arg_expr_type));
                        }
                    }
                    bind_type(&mut bindings, ret_type, None)
                }
                Type::Unknown => Ok(Type::Unknown),
                _ => Err(Error::CallNonFn(fn_type)),
            }
        }
        Expression::List(ref exprs) => {
            if exprs.is_empty() {
                return Ok(Type::List(box Type::Unknown));
            }

            let first = exprs.get(0).unwrap();
            let expected = try!(type_check(scope, &first));

            for expr in exprs.iter().skip(1) {
                let actual = try!(type_check(scope, &expr));
                if expected != actual {
                    return Err(Error::TypeMismatch(expected, actual));
                }
            }
            Ok(Type::List(box expected))
        }
        Expression::Tuple(ref exprs) => {
            let types = exprs.iter()
                .map(|expr| type_check(scope, expr))
                .collect();
            Ok(Type::Tuple(try!(types)))
        }

        Expression::Map(ref pairs) => {
            if pairs.is_empty() {
                return Ok(Type::Map(box (Type::Unknown, Type::Unknown)));
            }

            let &(ref key, ref val) = pairs.get(0).unwrap();
            let (key_typ, val_typ) = (try!(type_check(scope, &key)), try!(type_check(scope, &val)));

            for &(ref key, ref val) in pairs.iter().skip(1) {
                let actual_key_typ = try!(type_check(scope, &key));
                if actual_key_typ != key_typ {
                    return Err(Error::TypeMismatch(key_typ, actual_key_typ));
                }
                let actual_val_typ = try!(type_check(scope, &val));
                if actual_val_typ != val_typ {
                    return Err(Error::TypeMismatch(val_typ, actual_val_typ));
                }
            }
            Ok(Type::Map(box (key_typ, val_typ)))
        }
        Expression::Fn(ref args, ref exprs) => {
            Ok(Type::Fn(box try!(fn_type(scope,
                                         args,
                                         &exprs.iter().collect::<Vec<&Expression>>()))))
        }
        Expression::Symbol(ref sym) => {
            if scope.contains_key(sym) {
                Ok(scope.get(sym).unwrap())
            } else {
                Err(Error::UndefinedSymbol(sym.to_string()))
            }
        }
        Expression::Value(ref val) => value_to_type(scope, val),
    }
}
