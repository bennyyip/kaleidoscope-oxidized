use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate;

use std::collections::HashMap;

use parser::{Expr, Function, Prototype};
use lexer::Operator;

pub struct Compiler<'a> {
    pub context: &'a Context,
    pub builder: &'a Builder,
    pub module: &'a Module,
    pub fpm: &'a PassManager,
    variables: HashMap<String, PointerValue>,

    function: &'a Function,
    fn_val_opt: Option<FunctionValue>,
}

impl<'a> Compiler<'a> {
    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue, String> {
        match *expr {
            Expr::Number(n) => Ok(self.context.f64_type().const_float(n)),

            Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self.builder
                    .build_load(var, name.as_str())
                    .into_float_value()),
                None => Err("Could not find a matching variable.".to_owned()),
            },

            Expr::Binary {
                op,
                ref lhs,
                ref rhs,
            } => {
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                match op {
                    Operator::Add => Ok(self.builder.build_float_add(&lhs, &rhs, "tmpadd")),
                    Operator::Sub => Ok(self.builder.build_float_sub(&lhs, &rhs, "tmpsub")),
                    Operator::Mul => Ok(self.builder.build_float_mul(&lhs, &rhs, "tmpmul")),
                    Operator::Div => Ok(self.builder.build_float_div(&lhs, &rhs, "tmpdiv")),
                    Operator::LessThan => {
                        let cmp = self.builder.build_float_compare(
                            FloatPredicate::ULT,
                            &lhs,
                            &rhs,
                            "tmpcmp",
                        );
                        Ok(self.builder.build_unsigned_int_to_float(
                            &cmp,
                            &self.context.f64_type(),
                            "booltmp",
                        ))
                    }
                    Operator::GreaterThan => {
                        let cmp = self.builder.build_float_compare(
                            FloatPredicate::UGT,
                            &lhs,
                            &rhs,
                            "tmpcmp",
                        );
                        Ok(self.builder.build_unsigned_int_to_float(
                            &cmp,
                            &self.context.f64_type(),
                            "booltmp",
                        ))
                    }
                }
            }

            Expr::If {
                ref cond,
                ref consequence,
                ref alternative,
            } => {
                let zero = self.context.f64_type().const_float(0.0);
                let cond = self.compile_expr(cond)?;
                let cond =
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, &cond, &zero, "ifcond");

                let parent = self.fn_val();
                let then_bb = self.context.append_basic_block(&parent, "then");
                let else_bb = self.context.append_basic_block(&parent, "else");
                let cont_bb = self.context.append_basic_block(&parent, "ifcont");

                self.builder
                    .build_conditional_branch(&cond, &then_bb, &else_bb);

                // emit `then` block
                self.builder.position_at_end(&then_bb);
                let then_val = self.compile_expr(consequence)?;
                self.builder.build_unconditional_branch(&cont_bb);

                // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
                let then_bb = self.builder.get_insert_block().unwrap();

                // emit `else` block
                self.builder.position_at_end(&else_bb);
                let else_val = self.compile_expr(alternative)?;
                self.builder.build_unconditional_branch(&cont_bb);

                // Codegen of 'Else' can change the current block, update ThenBB for the PHI.
                let else_bb = self.builder.get_insert_block().unwrap();

                //emit merge block
                self.builder.position_at_end(&cont_bb);
                let phi = self.builder.build_phi(&self.context.f64_type(), "iftmp");
                phi.add_incoming(&[(&then_val, &then_bb), (&else_val, &else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }

            Expr::Call { ref name, ref args } => match self.get_function(name) {
                Some(func) => {
                    if args.len() as u32 != func.count_params() {
                        return Err(format!(
                            "Incorrect arguments passed, expected {}, got {}",
                            func.count_params(),
                            args.len()
                        ));
                    }
                    let mut argsv = Vec::with_capacity(args.len());

                    for arg in args {
                        argsv.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<&BasicValue> =
                        argsv.iter().by_ref().map(|x| x as &BasicValue).collect();

                    match self.builder
                        .build_call(&func, argsv.as_slice(), "calltmp", false)
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("Invalid call produced".to_string()),
                    }
                }
                None => Err(format!("Unknown function name: {}", name)),
            },
        }
    }

    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue, String> {
        let f64_type: &BasicType = &self.context.f64_type();
        let param_types = ::std::iter::repeat(f64_type)
            .take(proto.args.len())
            .collect::<Vec<&BasicType>>();
        let fn_type = self.context.f64_type().fn_type(&param_types, false);
        let fn_val = self.module.add_function(&proto.name, &fn_type, None);

        for (i, arg) in fn_val.params().enumerate() {
            arg.into_float_value().set_name(&proto.args[i]);
        }
        Ok(fn_val)
    }

    fn compile_function(&mut self) -> Result<FunctionValue, String> {
        let proto = &self.function.proto;
        let func = self.compile_prototype(proto)?;
        let entry = self.context.append_basic_block(&func, "entry");
        self.builder.position_at_end(&entry);

        self.fn_val_opt = Some(func);

        self.variables.reserve(proto.args.len());

        for (i, arg) in func.params().enumerate() {
            let arg_name = proto.args[i].as_str();

            let builder = self.context.create_builder();
            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(&entry),
            };
            let alloca = builder.build_alloca(&self.context.f64_type(), arg_name);
            self.builder.build_store(&alloca, &arg);

            self.variables.insert(proto.args[i].clone(), alloca);
        }

        let body = self.compile_expr(&self.function.body)?;

        self.builder.build_return(Some(&body));

        if func.verify(true) {
            self.fpm.run_on_function(&func);
            Ok(func)
        } else {
            unsafe {
                func.delete();
            }
            Err("Invalid generated function".to_string())
        }
    }

    fn get_function(&self, name: &str) -> Option<FunctionValue> {
        self.module.get_function(name)
    }

    fn fn_val(&self) -> FunctionValue {
        self.fn_val_opt.unwrap()
    }

    pub fn compile(
        context: &'a Context,
        builder: &'a Builder,
        fpm: &'a PassManager,
        module: &'a Module,
        function: &'a Function,
    ) -> Result<FunctionValue, String> {
        let mut compiler = Compiler {
            context: context,
            builder: builder,
            fpm: fpm,
            module: module,
            variables: HashMap::new(),
            function: function,
            fn_val_opt: None,
        };
        compiler.compile_function()
    }
}
