#![feature(nll)]

extern crate inkwell;
extern crate llvm_sys;
#[macro_use]
extern crate lazy_static;


pub mod lexer;
pub mod parser;
pub mod codegen;

pub use parser::ANONYMOUS_FUNCTION_NAME;

#[no_mangle]
#[allow(dead_code)]
pub extern "C" fn putchard(ch: f64) -> f64 {
    println!("{}", ch);
    0.0
}
