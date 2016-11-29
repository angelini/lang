#![feature(advanced_slice_patterns, box_patterns, box_syntax, plugin, slice_patterns)]
#![plugin(peg_syntax_ext)]

extern crate rand;
extern crate rustyline;

mod ast;
mod eval;
mod primitives;
mod scope;
mod types;

use ast::{Expression, Value};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use scope::{TypeScope, ValueScope};
use std::env;
use std::fs::File;
use std::mem;
use std::io;
use std::io::prelude::*;

peg_file! grammar("grammar.rustpeg");

fn verbose_eval(mut tscope: &mut TypeScope, mut vscope: &mut ValueScope, expr: Expression) -> bool {
    println!("exp: {:?}", expr);
    match types::type_check(tscope, &expr) {
        Ok(typ) => {
            println!("typ: {:?}", typ);
        }
        Err(e) => {
            println!("type error: {}", e);
            println!("expression: {:?}", expr);
            return false;
        }
    }

    match eval::eval(vscope, &expr) {
        Ok(val) => {
            println!("ret: {:?}", val);
        }
        Err(e) => {
            println!("eval error: {}", e);
            println!("expression: {:?}", expr);
            return false;
        }
    }
    println!("---");
    true
}

fn parse_and_eval(mut tscope: &mut TypeScope,
                  mut vscope: &mut ValueScope,
                  line: &str,
                  show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    let expr = match grammar::expression(&line) {
        Ok(expr) => expr,
        Err(err) => return println!("parse error: {:?}", err),
    };

    verbose_eval(tscope, vscope, expr);
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
                if !verbose_eval(tscope, vscope, expr) {
                    panic!()
                }
            }
        }
        Err(err) => panic!("parse error: {:?}", err),
    }
    Ok(())
}

const HISTORY_FILE: &'static str = "history.txt";

fn start_repl(mut tscope: &mut TypeScope, mut vscope: &mut ValueScope) {
    parse_and_eval(&mut tscope, &mut vscope, "let foo = 1", true);
    parse_and_eval(&mut tscope, &mut vscope, "let bar = [1, 2, foo]", true);
    parse_and_eval(&mut tscope,
                   &mut vscope,
                   "let identity = fn (id: T) { id }",
                   true);

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

fn eval_stream(mut tscope: &mut TypeScope, mut vscope: &mut ValueScope) {
    println!("====START====");
    let stdin = io::stdin();
    let mut lines = vec![];
    for line in stdin.lock().lines() {
        match line {
            Ok(l) => {
                if &l == "====EXEC====" {
                    match grammar::expressions(&lines.join("\n")) {
                        Ok(exprs) => {
                            for expr in exprs {
                                verbose_eval(&mut tscope, &mut vscope, expr);
                            }
                        }
                        Err(_) => panic!()
                    }
                    lines.clear();
                    println!("====END====");
                } else {
                    lines.push(l);
                }
            }
            Err(_) => panic!()
        }
    }
}

fn main() {
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());

    let args = env::args().collect::<Vec<String>>();

    let mut tscope = TypeScope::new();
    let mut vscope = ValueScope::new();
    primitives::add_primitive_fns(&mut tscope, &mut vscope);

    match args[1..] {
        [] => start_repl(&mut tscope, &mut vscope),
        [ref arg] => {
            if arg == "__stream__" {
                eval_stream(&mut tscope, &mut vscope)
            } else {
                eval_file(&mut tscope, &mut vscope, arg).unwrap()
            }
        }
        _ => panic!("Invalid process args {:?}", args),
    }
}
