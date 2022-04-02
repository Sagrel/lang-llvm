use anyhow::{Ok, Result};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{AggregateValue, BasicValue, BasicValueEnum, CallableValue},
};
use lang_frontend::{
    ast::{Anotated, Ast, Pattern},
    inferer::Inferer,
    token::Token,
    types::Type,
};

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue;

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    // TODO change this to a vector of hashmap to account for scoping?
    variables: HashMap<String, BasicValueEnum<'ctx>>,
    fn_stack: Vec<FunctionValue<'ctx>>,
    type_table: &'ctx [Type],
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, type_table: &'ctx [Type]) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("main_module");
        Self {
            context,
            builder,
            module,
            variables: HashMap::new(),
            fn_stack: Vec::new(),
            type_table,
        }
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        *self.fn_stack.last().unwrap()
    }

    pub fn compile_module(mut self, ast: Vec<Anotated<Ast>>) -> anyhow::Result<Module<'ctx>> {
        // TODO module.add_function("do_nothing", fn_type, None);
        for node in ast {
            self.compile(node)?;
        }
        Ok(self.module)
    }

    fn generate_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
        match Inferer::get_most_concrete_type(t, self.type_table) {
            Type::Text => self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Const)
                .into(),
            Type::Number => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Tuple(args) => {
                let args: Vec<_> = args.iter().map(|arg| self.generate_type(arg)).collect();
                self.context.struct_type(args.as_slice(), false).into()
            }
            // HACK Funtion types are turned into pointer types, that is not very nice?
            Type::Fn(args, ret) => {
                let param_types: Vec<_> = args
                    .iter()
                    .map(|arg| match self.generate_type(arg) {
                        BasicTypeEnum::ArrayType(x) => x.into(),
                        BasicTypeEnum::FloatType(x) => x.into(),
                        BasicTypeEnum::IntType(x) => x.into(),
                        BasicTypeEnum::PointerType(x) => x.into(),
                        BasicTypeEnum::StructType(x) => x.into(),
                        BasicTypeEnum::VectorType(x) => x.into(),
                    })
                    .collect();
                self.generate_type(ret.as_ref())
                    .fn_type(param_types.as_slice(), false)
                    .ptr_type(inkwell::AddressSpace::Global)
                    .into()
            }
            _ => unreachable!("{}", t),
        }
    }

    fn void_value(&self) -> BasicValueEnum<'ctx> {
        self.context
            .struct_type(&[], true)
            .const_zero()
            //.const_named_struct(&[])
            .into()
    }

    // Creates a prototype for the function
    fn compile_prototype(&mut self, name: &str, ty: &Type) -> Result<FunctionValue<'ctx>> {
        match ty {
            Type::Fn(args, ret) => {
                let ret_type = self.generate_type(ret.as_ref());
                let arg_types = args
                    .iter()
                    .map(|arg| self.generate_type(arg).into())
                    .collect::<Vec<_>>();
                let lambda_type = ret_type.fn_type(arg_types.as_slice(), false);

                Ok(self.module.add_function(name, lambda_type, None))
            }
            _ => Err(anyhow::anyhow!(
                "Cant generate prototype of this type {}",
                ty
            )),
        }
    }

    fn pattern_asigment(
        &mut self,
        (pattern, _, _): Anotated<Pattern>,
        value: Anotated<Ast>,
    ) -> anyhow::Result<()> {
        match pattern {
            Pattern::Var((name_tk, _)) => {
                let name = name_tk.to_string();
                if let (Ast::Lambda(args, _, body), _, Some(ty)) = value {
                    let prototype = self.compile_prototype(name.as_str(), &ty)?;
                    let value = self.compile_lambda(args, *body, prototype)?;

                    self.variables.insert(
                        name,
                        value
                            .as_global_value()
                            .as_pointer_value()
                            .as_basic_value_enum(),
                    );
                } else {
                    let value = self.compile(value)?.unwrap();
                    self.variables.insert(name, value);
                }
            }
            Pattern::Tuple(args) => {
                if let Type::Tuple(_) = value.2.clone().unwrap() {
                    let value = self.compile(value)?.unwrap();

                    for (index, pattern) in args.into_iter().enumerate() {
                        let atriv = self
                            .builder
                            .build_extract_value(value.into_struct_value(), index as u32, "atriv")
                            .unwrap();
                        // BIG HACK
                        self.variables.insert("__temp".to_string(), atriv);
                        let atriv = (
                            Ast::Variable((Token::Ident("__temp".to_string()), pattern.1.clone())),
                            pattern.1.clone(),
                            pattern.2.clone(),
                        );
                        self.pattern_asigment(pattern, atriv)?;
                    }
                } else {
                    return Err(anyhow::anyhow!("This should be a tuple, right?"));
                }
            }
        }
        Ok(())
    }

    fn compile_lambda(
        &mut self,
        args: Vec<Anotated<Ast>>,
        body: Anotated<Ast>,
        f: FunctionValue<'ctx>,
    ) -> Result<FunctionValue<'ctx>> {
        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);

        for (param_value, param) in f.get_param_iter().zip(args.into_iter()) {
            if let Ast::Declaration(pattern, _, _, _, _) = param.0 {
                // BIG HACK
                self.variables.insert("__temp".to_string(), param_value);
                let param_value = (
                    Ast::Variable((Token::Ident("__temp".to_string()), pattern.1.clone())),
                    pattern.1.clone(),
                    pattern.2.clone(),
                );
                self.pattern_asigment(pattern, param_value)?;
            }
        }

        // Stablish the new funtion body as the lambda body
        self.fn_stack.push(f);
        let is_void = body.2.as_ref().unwrap() == &Type::void();
        let value = self.compile(body)?;
        if !is_void {
            self.builder.build_return(Some(&value.unwrap()));
        } else {
            self.builder.build_return(None);
        }

        // Restore the current function if we compiled a nested function, don't do it if we just compiled a top level function, as there is no current function in that case
        self.fn_stack.pop();
        if let Some(current) = self.fn_stack.last() {
            self.builder
                .position_at_end(current.get_last_basic_block().unwrap());
        }

        if true {
            //HACK f.verify(true) {
            Ok(f)
        } else {
            Err(anyhow::anyhow!(
                "Failed function verification of this {:?}",
                f
            ))
        }
    }

    fn compile(
        &mut self,
        (node, span, ty): Anotated<Ast>,
    ) -> anyhow::Result<Option<BasicValueEnum<'ctx>>> {
        Ok(Some(match node {
            Ast::Error | Ast::Coment(_) => return Ok(None), // FIXME ignore this cases and return None or something,
            Ast::Literal((Token::Bool(b), _)) => self
                .context
                .bool_type()
                .const_int(if b { 1 } else { 0 }, false)
                .into(),
            Ast::Literal((Token::Number(n), _)) => {
                self.context.f64_type().const_float(n.parse()?).into()
            }
            Ast::Literal((Token::Text(t), _)) => {
                let string = self
                    .builder
                    .build_global_string_ptr(&(t.clone() + "\0"), t.as_str());

                string
                    .as_pointer_value()
                    .const_cast(
                        self.context
                            .i8_type()
                            .ptr_type(inkwell::AddressSpace::Const),
                    )
                    .into()
            }
            Ast::Variable((Token::Ident(name), _)) => *self
                .variables
                .get(&name)
                .ok_or_else(|| anyhow::anyhow!("Missing declaration for {}", name))?,
            // HACK This is a hack to get external functions woking fast
            Ast::Declaration(
                (Pattern::Var((Token::Ident(name), _)), _, _),
                _,
                Some(ty),
                _,
                None,
            ) => {
                if let (Ast::Type(ty), _, _) = *ty {
                    let prototype = self.compile_prototype(name.as_str(), &ty)?;

                    self.variables.insert(
                        name,
                        prototype
                            .as_global_value()
                            .as_pointer_value()
                            .as_basic_value_enum(),
                    );
                }

                self.void_value()
            }
            Ast::Declaration(pattern, _, _, _, Some(value)) => {
                self.pattern_asigment(pattern, *value)?;

                self.void_value()
            }
            Ast::Call(caller, args) => {
                let caller = self.compile(*caller)?.unwrap();
                let args = args
                    .into_iter()
                    .map(|arg| self.compile(arg).map(|res| res.unwrap().into()))
                    .collect::<Result<Vec<_>, _>>()?;
                // TODO see set_tail_call()
                // TODO handle void functions
                self.builder
                    .build_call(
                        CallableValue::try_from(caller.into_pointer_value()).unwrap(),
                        args.as_slice(),
                        "function call",
                    )
                    .try_as_basic_value()
                    .expect_left("Fuckkkkkk")
            }
            Ast::Binary(l, (Token::Op(op), _), r) => {
                let l = self.compile(*l)?.unwrap();
                let r = self.compile(*r)?.unwrap();
                // TODO completar un poco
                match (op.as_str(), self.generate_type(&ty.unwrap())) {
                    ("+", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_add(l.into_float_value(), r.into_float_value(), "add float")
                        .into(),
                    ("-", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_sub(l.into_float_value(), r.into_float_value(), "sub float")
                        .into(),
                    ("*", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_mul(l.into_float_value(), r.into_float_value(), "mul float")
                        .into(),
                    ("/", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_div(l.into_float_value(), r.into_float_value(), "div float")
                        .into(),
                    _ => todo!(),
                }
            }
            Ast::While(_, cond, body) => {
                let body_block = self
                    .context
                    .append_basic_block(self.fn_value(), "body_block");
                let after_block = self
                    .context
                    .append_basic_block(self.fn_value(), "after_block");

                // emit condition and first jump
                let cond = self.compile(*cond)?.unwrap();
                self.builder.build_conditional_branch(
                    cond.into_int_value(),
                    body_block,
                    after_block,
                );

                //  emit body and loop jump
                self.builder.position_at_end(body_block);
                self.compile(*body)?;
                self.builder.build_conditional_branch(
                    cond.into_int_value(),
                    body_block,
                    after_block,
                );

                let _body_block = self.builder.get_insert_block().unwrap(); // This adds the block?

                // after the loop
                self.builder.position_at_end(after_block);

                // FIXME what to return? If we make this function return an optional value or divide it into 2 diferent functions this can be solved.
                todo!();
            }
            Ast::If(_, cond, if_body, _, else_body) => {
                // build cond
                let cond = self.compile(*cond)?.unwrap();

                // branches
                let if_block = self.context.append_basic_block(self.fn_value(), "if_block");
                let else_block = self
                    .context
                    .append_basic_block(self.fn_value(), "else_block");
                let after_block = self
                    .context
                    .append_basic_block(self.fn_value(), "after_block");

                // emit the jump
                self.builder
                    .build_conditional_branch(cond.into_int_value(), if_block, else_block);

                //  if body
                self.builder.position_at_end(if_block);
                let if_value = self.compile(*if_body)?.unwrap();
                self.builder.build_unconditional_branch(after_block);

                let if_block = self.builder.get_insert_block().unwrap(); // This adds the block?

                //  else body
                self.builder.position_at_end(else_block);
                let else_value = self.compile(*else_body)?.unwrap();
                self.builder.build_unconditional_branch(after_block);

                let else_block = self.builder.get_insert_block().unwrap(); // This adds the block?

                // after the loop
                self.builder.position_at_end(after_block);

                // We create a phi value that return the value of the branch taken
                let phi = self
                    .builder
                    .build_phi(self.generate_type(&ty.unwrap()), "if");

                phi.add_incoming(&[(&if_value, if_block), (&else_value, else_block)]);

                phi.as_basic_value()
            }
            Ast::Tuple(args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.compile(arg))
                    .collect::<Result<Option<Vec<_>>, _>>()?
                    .unwrap();

                self.generate_type(&ty.unwrap())
                    .into_struct_type()
                    .const_named_struct(args.as_slice())
                    .into()
            }
            Ast::Block(nodes) => {
                let mut res = None;

                for node in nodes {
                    res = self.compile(node)?;
                }
                // TODO this should return nothing instead of empty struct
                res.unwrap_or_else(|| self.void_value())
            }
            // This is only for anonymous lambdas, named ones are handled with definitions
            Ast::Lambda(args, _, body) => {
                let prototype = self.compile_prototype("anonymous", ty.as_ref().unwrap())?;
                let value = self.compile_lambda(args, *body, prototype)?;
                value.as_global_value().as_pointer_value().into()
            }
            _ => return Err(anyhow::anyhow!("Unexpected node at {:?}", span)),
        }))
    }
}
