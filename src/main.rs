extern crate inkwell;
extern crate kaleidoscope;

use std::io;
use std::io::prelude::*;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

use kaleidoscope::parser::*;
use kaleidoscope::lexer::{Lexer, Token};
use kaleidoscope::codegen::Compiler;

fn main() {
    let mut display_lexer_output = false;
    let mut display_parser_output = false;
    let mut display_compiler_output = false;
    let mut no_optimization = false;

    for arg in ::std::env::args() {
        match arg.as_str() {
            "--dl" => display_lexer_output = true,
            "--dp" => display_parser_output = true,
            "--dc" => display_compiler_output = true,
            "-0" => no_optimization = true,
            _ => (),
        }
    }

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // FPM
    let fpm = PassManager::create_for_function(&module);

    if !no_optimization {
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
    }

    fpm.initialize();

    // Target
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target.");

    let mut prev_exprs: Vec<Box<Function>> = vec![];

    loop {
        print!("?> ");
        std::io::stdout()
            .flush()
            .expect("Could not flush to standard output.");
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read from stdin");

        if input.starts_with("exit") || input.starts_with("quit") {
            break;
        } else if input.chars().all(char::is_whitespace) {
            continue;
        }

        if display_lexer_output {
            println!(
                "-> Lexer input: \n{:?}\n",
                Lexer::new(input.as_str()).collect::<Vec<Token>>()
            );
        }

        let module = context.create_module("tmp");

        let (name, is_anonymous) = match Parser::from_source(input.as_str()).parse() {
            Ok(function) => {
                if display_parser_output {
                    println!("-> paser output:\n{:?}\n", function);
                }

                for expr in &prev_exprs {
                    Compiler::compile(&context, &builder, &fpm, &module, expr)
                        .expect("Cannot re-add previously compiled function.");
                }

                match Compiler::compile(&context, &builder, &fpm, &module, &function) {
                    Ok(func) => {
                        if display_compiler_output {
                            println!("-> IR:");
                            func.print_to_stderr();
                        }
                        let is_anonymous = function.is_anonymous();
                        if !is_anonymous {
                            prev_exprs.push(function);
                        }
                        (func.get_name().to_str().unwrap().to_string(), is_anonymous)
                    }
                    Err(err) => {
                        println!("!> Error when emiting LLVM IR: {:?}", err);
                        continue;
                    }
                }
            }
            Err(err) => {
                println!("!> Error during parsing: {:?}", err);
                continue;
            }
        };

        // JIT
        if is_anonymous {
            let engine = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            let addr = match engine.get_function_address(&name) {
                Ok(addr) => addr,
                Err(err) => {
                    println!("!> Error during execution: {:?}", err);
                    continue;
                }
            };
            let compiled_fn: extern "C" fn() -> f64 = unsafe { std::mem::transmute(addr) };
            println!("=> {}", compiled_fn());
        }
    }
}
