#![feature(nll)]

extern crate inkwell;
extern crate llvm_sys;

pub mod lexer;
pub mod parser;
pub mod codegen;
