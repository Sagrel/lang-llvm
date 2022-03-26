use compiler::Compiler;
use inkwell::{context::Context, OptimizationLevel};
use lang_frontend::{
    ast::{Anotated, Ast},
    types::Type,
};

mod compiler;

pub fn compile_and_run(ast: Vec<Anotated<Ast>>, type_table: &[Type]) -> anyhow::Result<()> {
    let context = Context::create();

    let compiler = Compiler::new(&context, type_table);

    let module = compiler.compile_module(ast)?;

	module.print_to_stderr();

    let jit = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main = unsafe { jit.get_function::<unsafe extern "C" fn() -> f64>("main")? };

    unsafe {
        println!("Main returned {}", main.call());
    }
    Ok(())
}
