use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate;
use inkwell::module::Linkage;
use std::collections::HashMap;

use parser::{Expr, Function, Prototype};

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

            Expr::Unary { op, ref operand } => {
                let operand = self.compile_expr(operand)?;
                let func = match self.get_function(&format!("unary{}", op)) {
                    Some(func) => func,
                    None => return Err(format!("cannot find unary opeartor {}", op)),
                };
                match self.builder
                    .build_call(&func, &[&operand], "unaryop", false)
                    .left()
                {
                    Some(value) => Ok(value.into_float_value()),
                    None => Err("Invalid call produced".to_string()),
                }
            }

            Expr::Binary {
                op,
                ref lhs,
                ref rhs,
            } => {
                // Special case '=' because we don't want to emit the LHS as an expression.
                if op == '=' {
                    match lhs.as_ref() {
                        &Expr::Variable(ref var_name) => {
                            let val = self.compile_expr(rhs)?;
                            let variable = self.variables.get(var_name);
                            if variable.is_none() {
                                return Err(format!("unknown variable name: {}", var_name));
                            }
                            self.builder.build_store(&variable.unwrap(), &val);
                            return Ok(val);
                        }
                        other => {
                            return Err(format!(
                                "destination of `=` must be a variable, got {:?}",
                                other
                            ))
                        }
                    }
                }

                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                match op {
                    '+' => Ok(self.builder.build_float_add(&lhs, &rhs, "tmpadd")),
                    '-' => Ok(self.builder.build_float_sub(&lhs, &rhs, "tmpsub")),
                    '*' => Ok(self.builder.build_float_mul(&lhs, &rhs, "tmpmul")),
                    '/' => Ok(self.builder.build_float_div(&lhs, &rhs, "tmpdiv")),
                    '<' => {
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
                    op => {
                        let func = match self.get_function(&format!("binary{}", op)) {
                            Some(func) => func,
                            None => return Err(format!("cannot find binary opeartor {}", op)),
                        };
                        match self.builder
                            .build_call(&func, &[&lhs, &rhs], "binop", false)
                            .left()
                        {
                            Some(value) => Ok(value.into_float_value()),
                            None => Err("Invalid call produced".to_string()),
                        }
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

            Expr::For {
                ref var_name,
                ref start,
                ref end,
                ref step,
                ref body,
            } => {
                let parent = self.fn_val();
                // Create an alloca for the variable in the entry block.
                let alloca = self.create_entry_block_alloca(var_name, None);
                // Emit the start code first, without 'variable' in scope.
                let start_val = self.compile_expr(start)?;

                // Store the value into the alloca.
                self.builder.build_store(&alloca, &start_val);

                // Make the new basic block for the loop header, inserting after current
                // block.
                let loop_bb = self.context.append_basic_block(&parent, "loop");

                // Insert an explicit fall through from the current block to the LoopBB.
                self.builder.build_unconditional_branch(&loop_bb);

                // Start insertion in LoopBB.
                self.builder.position_at_end(&loop_bb);

                // Within the loop, the variable is defined equal to the PHI node.  If it
                // shadows an existing variable, we have to restore it, so save it now.
                let old_val = self.variables.remove(var_name);
                self.variables.insert(var_name.to_string(), alloca);

                // Emit the body of the loop.  This, like any other expr, can change the
                // current BB.  Note that we ignore the value computed by the body, but don't
                // allow an error.
                self.compile_expr(body)?;

                let step_val = match *step {
                    Some(ref step) => self.compile_expr(step)?,
                    // If not specified, use 1.0.
                    None => self.context.f64_type().const_float(1.0),
                };

                // Compute the end condition.
                let end_cond = &self.compile_expr(end)?;

                // Reload, increment, and restore the alloca.  This handles the case where
                // the body of the loop mutates the variable.
                let curr_var = self.builder.build_load(&alloca, &var_name);
                let next_var = self.builder.build_float_add(
                    &curr_var.into_float_value(),
                    &step_val,
                    "nextvar",
                );
                self.builder.build_store(&alloca, &next_var);

                // Convert condition to a bool by comparing non-equal to 0.0.
                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    &end_cond,
                    &self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                // Create the "after loop" block and insert it.
                let after_bb = self.context.append_basic_block(&parent, "afterloop");

                // Insert the conditional branch into the end of LoopEndBB.
                self.builder
                    .build_conditional_branch(&end_cond, &loop_bb, &after_bb);

                // Any new code will be inserted in AfterBB.
                self.builder.position_at_end(&after_bb);

                // Restore the unshadowed variable.
                self.variables.remove(var_name);
                if let Some(x) = old_val {
                    self.variables.insert(var_name.to_string(), x);
                }

                // for expr always returns 0.0.
                Ok(self.context.f64_type().const_float(0.0))
            }

            Expr::Var {
                ref var_names,
                ref body,
            } => {
                let mut old_bindings = vec![];
                for &(ref name, ref val) in var_names {
                    let val = self.compile_expr(val)?;
                    let alloca = self.create_entry_block_alloca(name, None);
                    self.builder.build_store(&alloca, &val);

                    old_bindings.push(self.variables.remove(name));
                    self.variables.insert(name.to_owned(), alloca);
                }

                let body = self.compile_expr(body)?;

                for (i, &(ref name, ref _val)) in var_names.iter().enumerate() {
                    if let Some(alloca) = old_bindings[i] {
                        *self.variables.get_mut(name).unwrap() = alloca;
                    }
                }

                Ok(body)
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

        self.variables.clear();
        self.variables.reserve(proto.args.len());

        for (i, arg) in func.params().enumerate() {
            let arg_name = proto.args[i].as_str();

            let alloca = self.create_entry_block_alloca(arg_name, Some(&entry));

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

    /// Cretes a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str, entry: Option<&BasicBlock>) -> PointerValue {
        let builder = self.context.create_builder();

        let owned_entry = self.fn_val().get_entry_basic_block();
        let entry = owned_entry.as_ref().or(entry).unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(&self.context.f64_type(), name)
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

    pub fn compile_extern(
        context: &'a Context,
        module: &'a Module,
        proto: &'a Prototype,
    ) -> Result<(), String> {
        // TODO: move to codegen
        let f64_type: &BasicType = &context.f64_type();
        let param_types = ::std::iter::repeat(f64_type)
            .take(proto.args.len())
            .collect::<Vec<&BasicType>>();
        let fn_type = context.f64_type().fn_type(&param_types, false);
        let fn_val = module.add_function(&proto.name, &fn_type, Some(&Linkage::ExternalLinkage));

        for (i, arg) in fn_val.params().enumerate() {
            arg.into_float_value().set_name(&proto.args[i]);
        }

        if !fn_val.verify(true) {
            unsafe {
                fn_val.delete();
            }
            Err(format!("Invalid generated function `{}`", proto.name))
        } else {
            Ok(())
        }
    }
}
