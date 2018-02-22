extern crate inkwell;
extern crate kaleidoscope;
#[macro_use]
extern crate structopt;

use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::fs::File;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::OptimizationLevel;

use inkwell::module::Linkage;
use inkwell::types::BasicType;

use kaleidoscope::parser::*;
use kaleidoscope::lexer::{Lexer, Token};
use kaleidoscope::codegen::Compiler;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "kaleidoscope REPL")]
struct Opt {
    #[structopt(long = "dl", help = "Prints lexer output")]
    display_lexer_output: bool,
    #[structopt(long = "dp", help = "Prints parser output")]
    display_parser_output: bool,
    #[structopt(long = "dc", help = "Prints LLVM IR")]
    display_compiler_output: bool,
    #[structopt(short = "0", help = "Diable LLVM function pass managers")]
    no_optimization: bool,
    #[structopt(short = "i", long = "input", parse(from_os_str), help = "input file")]
    input: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // FPM
    let fpm = PassManager::create_for_function(&module);

    if !opt.no_optimization {
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

    if let Some(ref input) = opt.input {
        match File::open(input) {
            Ok(mut file) => {
                let mut content = String::new();
                file.read_to_string(&mut content).unwrap();
                let mut parser = Parser::from_source(&content);
                loop {
                    match parser.current() {
                        Some(Token::Def) => match handle_definition(&opt, &mut parser) {
                            Ok(function) => prev_defs.push(function),
                            Err(err) => {
                                println!("{}", err);
                                std::process::exit(1);
                            }
                        },

                        Some(Token::Extern) => match parser.parse_extern() {
                            Ok(proto) => prev_externs.push(proto),
                            Err(err) => {
                                println!("!> Error when emiting LLVM IR for extern def: {:?}", err);
                                std::process::exit(1);
                            }
                        },

                        Some(Token::Delimiter) => {
                            parser.get_next_token();
                        }

                        None => {
                            break;
                        }

                        _ => match handle_top_level(
                            &opt,
                            &mut parser,
                            &context,
                            &builder,
                            &fpm,
                            &module,
                        ) {
                            Ok(_) => (),
                            Err(err) => {
                                println!("{}", err);
                                std::process::exit(1);
                            }
                        },
                    }
                }
            }
            Err(err) => {
                println!("Cannot open file: {:?}", err);
                std::process::exit(1);
            }
        }
    }

    // REPL
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

        if opt.display_lexer_output {
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

            if !fn_val.verify(true) {
                unsafe {
                    fn_val.delete();
                }
                println!("Invalid generated function");
            }
        }

        // compile functions
        for expr in &prev_defs {
            match Compiler::compile(&context, &builder, &fpm, &module, expr) {
                Ok(_) => (),
                Err(err) => println!(
                    "!> Error when emiting LLVM IR for function definition: {:?}",
                    err
                ),
            }
        }

        let mut parser = Parser::from_source(input.as_str());

        match parser.current() {
            Some(Token::Def) => match handle_definition(&opt, &mut parser) {
                Ok(function) => {
                    if opt.display_compiler_output {
                        match Compiler::compile(&context, &builder, &fpm, &module, &function) {
                            Ok(func) => {
                                println!("-> IR:");
                                func.print_to_stderr();
                            }
                            Err(err) => println!(
                                "!> Error when emiting LLVM IR for function definition: {:?}",
                                err
                            ),
                        }
                    }

                    prev_defs.push(function);
                }
                Err(err) => println!("{}", err),
            },

            Some(Token::Extern) => match parser.parse_extern() {
                Ok(proto) => prev_externs.push(proto),
                Err(err) => println!("!> Error when emiting LLVM IR for extern def: {:?}", err),
            },

            _ => match handle_top_level(&opt, &mut parser, &context, &builder, &fpm, &module) {
                Ok(result) => println!("=> {}", result),
                Err(err) => println!("{}", err),
            },
        }
    }
}

#[allow(dead_code)]
#[link(name = "ksio")]
extern "C" {
    fn putchard(x: f64) -> f64;
    fn printf(x: f64) -> f64;
}

fn handle_top_level(
    opt: &Opt,
    parser: &mut Parser,
    context: &Context,
    builder: &Builder,
    fpm: &PassManager,
    module: &Module,
) -> Result<f64, String> {
    match parser.parse_top_level() {
        Ok(function) => {
            if opt.display_parser_output {
                println!("-> paser output:\n{:?}\n", function);
            }

            match Compiler::compile(&context, &builder, &fpm, &module, &function) {
                Ok(func) => {
                    if opt.display_compiler_output {
                        println!("-> IR:");
                        func.print_to_stderr();
                    }
                }
                Err(err) => {
                    return Err(format!(
                        "!> Error when emiting LLVM IR for top level: {:?}",
                        err
                    ))
                }
            }

            // execute code
            let engine = module
                .create_jit_execution_engine(OptimizationLevel::None)
                .unwrap();

            let addr = match engine.get_function_address(ANONYMOUS_FUNCTION_NAME) {
                Ok(addr) => addr,
                Err(err) => return Err(format!("!> Error during execution: {:?}", err)),
            };
            let compiled_fn: extern "C" fn() -> f64 = unsafe { std::mem::transmute(addr) };
            Ok(compiled_fn())
        }
        Err(err) => Err(format!("!> Error during parsing top level: {:?}", err)),
    }
}

fn handle_definition(opt: &Opt, parser: &mut Parser) -> Result<Box<Function>, String> {
    match parser.parse_definition() {
        Ok(function) => {
            if opt.display_parser_output {
                println!("-> paser output:\n{:?}\n", function);
            }
            Ok(function)
        }
        Err(err) => Err(format!(
            "!> Error during parsing function definition: {:?}",
            err
        )),
    }
}
