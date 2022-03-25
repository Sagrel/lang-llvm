extern crate lang_frontend;

extern crate inkwell;
use std::{collections::HashMap, env, fs};

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, CallableValue},
};
use lang_frontend::{
    ast::{Anotated, Ast, Declaration},
    inferer::Inferer,
    parse_file,
    token::Token,
    types::Type,
};

use self::inkwell::builder::Builder;
use self::inkwell::context::Context;
use self::inkwell::module::Module;
use self::inkwell::values::{FunctionValue, PointerValue};
use self::inkwell::OptimizationLevel;
pub struct Compiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
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
            fn_value_opt: None,
            type_table,
        }
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    pub fn compile_module(mut self, ast: Vec<Anotated<Ast>>) -> anyhow::Result<Module<'ctx>> {
        // TODO module.add_function("do_nothing", fn_type, None);
        for node in ast {
            self.compile(node)?;
        }
        Ok(self.module)
    }

    // TODO Funtion types are turned into pointer types, that is not very nice
    fn generate_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
        match Inferer::get_most_concrete_type(t, self.type_table) {
            Type::Text => todo!(), // TODO This should be a vector type  self.context.const_string(string, null_terminated)
            Type::Number => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Tuple(args) => {
                let args: Vec<_> = args
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
                self.context.struct_type(args.as_slice(), false).into()
            }
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
            _ => unreachable!(),
        }
    }

    fn compile(&mut self, (node, span, ty): Anotated<Ast>) -> anyhow::Result<BasicValueEnum<'ctx>> {
        Ok(match node {
            Ast::Error => todo!(),
            Ast::Coment(_) => todo!(),
            Ast::Literal((Token::Bool(b), _)) => self
                .context
                .bool_type()
                .const_int(if b { 1 } else { 0 }, false)
                .into(),
            Ast::Literal((Token::Number(n), _)) => {
                self.context.f64_type().const_float(n.parse()?).into()
            }
            Ast::Literal((Token::Text(t), _)) => self
                .context
                .const_string(t.as_bytes(), false /* TODO investigate this */)
                .into(),
            Ast::Variable((Token::Ident(name), _)) => self.builder.build_load(
                *self
                    .variables
                    .get(&name)
                    .ok_or_else(|| anyhow::anyhow!("Missing declaration for {}", name))?,
                name.as_str(),
            ),
            Ast::Declaration((Token::Ident(name), _), variant) => {
                match *variant {
                    Declaration::Complete(_, _) => todo!(),
                    Declaration::OnlyType(_) => todo!(),
                    Declaration::OnlyValue(value, _) => {
                        // TODO If it's a function add it to the module funtion list
                        let ty = self.generate_type(value.2.as_ref().unwrap());
                        let value = self.compile(value)?;
                        self.variables.insert(name, value.into_pointer_value());

                        // TODO do not return weird void
                        self.context
                            .struct_type(&[], true)
                            .const_named_struct(&[])
                            .into()
                    }
                }
            }
            Ast::Call(caller, args) => {
                let caller = self.compile(*caller)?;
                let args = args
                    .into_iter()
                    .map(|arg| self.compile(arg).map(|res| res.into()))
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
                let l = self.compile(*l)?;
                let r = self.compile(*r)?;
                // TODO completar un poco
                match (op.as_str(), self.generate_type(&ty.unwrap())) {
                    ("+", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_add(l.into_float_value(), r.into_float_value(), "add float")
                        .into(),
                    ("-", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_sub(l.into_float_value(), r.into_float_value(), "add float")
                        .into(),
                    ("*", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_mul(l.into_float_value(), r.into_float_value(), "add float")
                        .into(),
                    ("/", BasicTypeEnum::FloatType(_)) => self
                        .builder
                        .build_float_div(l.into_float_value(), r.into_float_value(), "add float")
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
                let cond = self.compile(*cond)?;
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

                // TODO what to return?
                todo!();
            }
            Ast::If(_, cond, if_body, _, else_body) => {
                // build cond
                let cond = self.compile(*cond)?;

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
                let if_value = self.compile(*if_body)?;
                self.builder.build_unconditional_branch(after_block);

                let if_block = self.builder.get_insert_block().unwrap(); // This adds the block?

                //  else body
                self.builder.position_at_end(else_block);
                let else_value = self.compile(*else_body)?;
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
                    .collect::<Result<Vec<_>, _>>()?;

                self.generate_type(&ty.unwrap())
                    .into_struct_type()
                    .const_named_struct(args.as_slice())
                    .into()
            }
            Ast::Block(nodes) => {
                let mut res = None;

                for node in nodes {
                    res = Some(self.compile(node)?);
                }
                // TODO this should return nothing instead of empty struct
                res.unwrap_or_else(|| {
                    self.context
                        .struct_type(&[], true)
                        .const_named_struct(&[])
                        .into()
                })
            }
            Ast::Lambda(args, _, body) => {
                let ret_type = self.generate_type(&body.2.unwrap());
                let arg_types = args
                    .into_iter()
                    .map(|arg| self.generate_type(&arg.2.unwrap()).into())
                    .collect::<Vec<_>>();
                let lambda_type = ret_type.fn_type(arg_types.as_slice(), false);
                // TODO give it a name
                let lambda_value = self.module.add_function("anonymous", lambda_type, None);
                // TODO is this ok?
                lambda_value.as_global_value().as_pointer_value().into()
            }
            _ => return Err(anyhow::anyhow!("Unexpected node at {:?}", span)),
        })
    }
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 1] = [printd];

pub fn main() -> anyhow::Result<()> {
    let path = if let Some(path) = env::args().nth(1) {
        path
    } else {
        "./examples/simple.lang".to_owned()
    };

    let src = fs::read_to_string(path)?;
    parse_file(&src);

    let (_, ast_and_type_table, errors) = parse_file(&src);

    // TODO use crate thiserror or even better the anyhow crate
    if !errors.is_empty() {
        return Err(anyhow::anyhow!("Found errors"));
    }

    let (ast, type_table) = if let Some(x) = ast_and_type_table {
        x
    } else {
        return Err(anyhow::anyhow!("Could not parse Ast"));
    };

    let context = Context::create();

    let compiler = Compiler::new(&context, &type_table);

    let module = compiler.compile_module(ast);

    let jit = module?
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main = unsafe { jit.get_function::<unsafe extern "C" fn() -> f64>("main") }
        .expect("Main function not found");

    unsafe {
        println!("Main returned {}", main.call());
    }

    Ok(())
}
