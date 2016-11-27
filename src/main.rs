#![feature(advanced_slice_patterns, box_patterns, plugin, slice_patterns)]
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
use std::io::prelude::*;

peg_file! grammar("grammar.rustpeg");

fn parse_and_eval(mut tscope: &mut TypeScope,
                  mut vscope: &mut ValueScope,
                  line: &str,
                  show_line: bool) {
    if show_line {
        println!("lin: {:?}", line);
    }
    match grammar::expression(&line) {
        Ok(e) => {
            println!("typ: {:?}", types::type_check(tscope, e.clone()));
            println!("ret: {:?}", eval::eval(vscope, e));
            println!("scp: {:?}", vscope);
        }
        Err(err) => println!("parse error: {:?}", err),
    }
    println!("---")
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
                println!("exp: {:?}", expr);
                println!("typ: {:?}", types::type_check(tscope, expr.clone()));
                println!("ret: {:?}", eval::eval(vscope, expr));
                println!("---")
            }
        }
        Err(err) => panic!("parse error: {:?}", err),
    }
    Ok(())
}

const HISTORY_FILE: &'static str = "history.txt";

fn start_repl(mut tscope: &mut TypeScope, mut vscope: &mut ValueScope) {
    parse_and_eval(&mut tscope, &mut vscope, "foo = 1", true);
    parse_and_eval(&mut tscope, &mut vscope, "bar = [1, 2, foo]", true);
    parse_and_eval(&mut tscope,
                   &mut vscope,
                   "identity = fn (id: T) { id }",
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

fn main() {
    println!("mem::size_of::<Value>(): {:?}", mem::size_of::<Value>());
    println!("mem::size_of::<Expression>(): {:?}",
             mem::size_of::<Expression>());

    let args = env::args().collect::<Vec<String>>();

    let mut tscope = TypeScope::new();
    let mut vscope = ValueScope::new();
    primitives::add_primitive_fns(&mut tscope, &mut vscope);

    match args[1..] {
        [ref file] => eval_file(&mut tscope, &mut vscope, file).unwrap(),
        _ => start_repl(&mut tscope, &mut vscope),
    }
}
