extern crate inkwell;
extern crate kaleidoscope;

use std::io;
use std::io::prelude::*;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::OptimizationLevel;

use inkwell::module::Linkage;
use inkwell::types::BasicType;

use kaleidoscope::parser::*;
use kaleidoscope::lexer::{Lexer, Token};
use kaleidoscope::codegen::Compiler;

#[allow(dead_code)]
#[link(name = "ksio")]
extern "C" {
    fn putchard(x: f64) -> f64;
}

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

    let mut prev_defs: Vec<Box<Function>> = vec![];
    let mut prev_externs: Vec<Box<Prototype>> = vec![];

    // Repl
    loop {
        print!("?> ");
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        handle.flush().expect("Could not flush to standard output.");

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

        // compile externs
        for proto in &prev_externs {
            // TODO: move to codegen
            let f64_type: &BasicType = &context.f64_type();
            let param_types = ::std::iter::repeat(f64_type)
                .take(proto.args.len())
                .collect::<Vec<&BasicType>>();
            let fn_type = context.f64_type().fn_type(&param_types, false);
            let fn_val =
                module.add_function(&proto.name, &fn_type, Some(&Linkage::ExternalLinkage));

            for (i, arg) in fn_val.params().enumerate() {
                arg.into_float_value().set_name(&proto.args[i]);
            }
        }

        // compile functions
        for expr in &prev_defs {
            Compiler::compile(&context, &builder, &fpm, &module, expr)
                .expect("Cannot re-add previously compiled function.");
        }


        let mut parser = Parser::from_source(input.as_str());

        match parser.current() {
            Some(Token::Def) => match parser.parse_definition() {
                Ok(function) => {
                    if display_parser_output {
                        println!("-> paser output:\n{:?}\n", function);
                    }

                    match Compiler::compile(&context, &builder, &fpm, &module, &function) {
                        Ok(func) => {
                            if display_compiler_output {
                                println!("-> IR:");
                                func.print_to_stderr();
                            }
                            // add to prev_defs
                            prev_defs.push(function);
                        }
                        Err(err) => {
                            println!(
                                "!> Error when emiting LLVM IR for function definition: {:?}",
                                err
                            );
                            continue;
                        }
                    }
                }
                Err(err) => {
                    println!("!> Error during parsing function definition: {:?}", err);
                    continue;
                }
            },

            Some(Token::Extern) => match parser.parse_extern() {
                Ok(proto) => {
                    prev_externs.push(proto);
                }
                Err(err) => {
                    println!("!> Error when emiting LLVM IR for extern def: {:?}", err);
                    continue;
                }
            },

            _ => match parser.parse_top_level() {
                Ok(function) => {
                    if display_parser_output {
                        println!("-> paser output:\n{:?}\n", function);
                    }

                    match Compiler::compile(&context, &builder, &fpm, &module, &function) {
                        Ok(func) => {
                            if display_compiler_output {
                                println!("-> IR:");
                                func.print_to_stderr();
                            }
                        }
                        Err(err) => {
                            println!("!> Error when emiting LLVM IR for top level: {:?}", err);
                            continue;
                        }
                    }

                    // execute code
                    let engine = module
                        .create_jit_execution_engine(OptimizationLevel::None)
                        .unwrap();

                    let addr = match engine.get_function_address(ANONYMOUS_FUNCTION_NAME) {
                        Ok(addr) => addr,
                        Err(err) => {
                            println!("!> Error during execution: {:?}", err);
                            continue;
                        }
                    };
                    let compiled_fn: extern "C" fn() -> f64 = unsafe { std::mem::transmute(addr) };
                    println!("=> {}", compiled_fn());
                }
                Err(err) => {
                    println!("!> Error during parsing top level: {:?}", err);
                    continue;
                }
            },
        }
    }
}
