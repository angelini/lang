#![feature(advanced_slice_patterns, box_patterns, box_syntax, plugin, slice_patterns)]

#![plugin(clippy)]
#![plugin(peg_syntax_ext)]

extern crate rand;
extern crate rustyline;

mod ast;
mod eval;
mod primitives;
mod scope;
mod types;

use ast::{Expression, Type, Value};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scope::{TypeScope, ValueScope};
use std::env;
use std::fs::File;
use std::mem;
use std::io;
use std::io::prelude::*;

peg_file! grammar("grammar.rustpeg");

fn type_check_and_eval(mut tscope: &mut TypeScope,
                       mut vscope: &mut ValueScope,
                       expr: Expression,
                       verbose: bool)
                       -> bool {
    if verbose {
        println!("exp: {:?}", expr);
    }

    match types::type_check(tscope, &expr) {
        Ok(typ) => {
            if verbose {
                println!("typ: {:?}", typ);
            }
        }
        Err(e) => {
            println!("err: {}", e);
            return false;
        }
    }

    match eval::eval(vscope, &expr) {
        Ok(val) => {
            if verbose {
                println!("ret: {:?}", val);
            }
        }
        Err(e) => {
            println!("err: {}", e);
            return false;
        }
    }

    if verbose {
        println!("---");
    }
    true
}

fn eval_file(tscope: &mut TypeScope,
             vscope: &mut ValueScope,
             file: &str)
             -> Result<(), std::io::Error> {
    let mut f = try!(File::open(file));
    let mut contents = String::new();
    try!(f.read_to_string(&mut contents));

    match grammar::expressions(contents.trim()) {
        Ok(exprs) => {
            for expr in exprs {
                if !type_check_and_eval(tscope, vscope, expr, false) {
                    panic!()
                }
            }
        }
        Err(err) => panic!("parse error: {:?}", err),
    }
    Ok(())
}

struct Env {
    types: TypeScope,
    values: ValueScope,
    stdlibs: Vec<String>,
}

impl Env {
    fn new(stdlibs: &[&str]) -> Env {
        let mut env = Env {
            types: TypeScope::new(),
            values: ValueScope::new(),
            stdlibs: stdlibs.iter().map(|s| s.to_string()).collect(),
        };
        env.reset();
        env
    }

    fn reset(&mut self) {
        self.types = TypeScope::new();
        self.values = ValueScope::new();

        primitives::add_primitive_fns(&mut self.types, &mut self.values).unwrap();
        for lib in &self.stdlibs {
            eval_file(&mut self.types, &mut self.values, lib).unwrap()
        }

        self.types.descend();
        self.values.descend();
    }
}

fn parse_and_eval(env: &mut Env, line: &str, show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    let expr = match grammar::expression(line) {
        Ok(expr) => expr,
        Err(err) => return println!("parse error: {:?}", err),
    };

    type_check_and_eval(&mut env.types, &mut env.values, expr, true);
}

const HISTORY_FILE: &'static str = "history.txt";

fn start_repl(env: &mut Env) {
    let mut rl = Editor::<()>::new();
    if rl.load_history(HISTORY_FILE).is_err() {
        println!("No previous history");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                parse_and_eval(env, &line, false);
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

fn eval_buffer(env: &mut Env, buffer: &str) {
    match grammar::expressions(buffer) {
        Ok(exprs) => {
            for expr in exprs {
                type_check_and_eval(&mut env.types, &mut env.values, expr, true);
            }
        }
        Err(_) => panic!(),
    }
}

fn eval_stream(env: &mut Env) {
    println!("====START====");
    let stdin = io::stdin();
    let mut lines = vec![];
    for line in stdin.lock().lines() {
        match line {
            Ok(l) => {
                match l.as_str() {
                    "====EXEC====" => {
                        eval_buffer(env, &lines.join("\n"));
                        lines.clear();
                        println!("====END====");
                    }
                    "====RESET====" => env.reset(),
                    _ => lines.push(l),
                }
            }
            Err(_) => panic!(),
        }
    }
}

fn main() {
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());
    println!("mem::size_of::<Type>(): {:?}", mem::size_of::<Type>());
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());

    let args = env::args().collect::<Vec<String>>();
    let mut env = Env::new(&["stdlib.lang"]);

    match args[1..] {
        [] => start_repl(&mut env),
        [ref arg] => {
            if arg == "__stream__" {
                eval_stream(&mut env)
            } else {
                eval_file(&mut env.types, &mut env.values, arg).unwrap()
            }
        }
        _ => panic!("Invalid process args {:?}", args),
    }
}
