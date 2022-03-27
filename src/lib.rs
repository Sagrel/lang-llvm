use compiler::Compiler;
use inkwell::{context::Context, OptimizationLevel};
use lang_frontend::{
    ast::{Anotated, Ast},
    types::Type,
};

mod compiler;

pub fn compile_and_run<T>(
    ast: Vec<Anotated<Ast>>,
    type_table: &[Type],
    print_module: bool,
) -> anyhow::Result<T> {
    let context = Context::create();

    let compiler = Compiler::new(&context, type_table);

    let module = compiler.compile_module(ast)?;

    if print_module {
        module.print_to_stderr();
    }

    let jit = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let main = unsafe { jit.get_function::<unsafe extern "C" fn() -> T>("main")? };

    let res = unsafe { main.call() };
    Ok(res)
}

#[cfg(test)]
mod tests {
    use lang_frontend::parse_file;

    use crate::compile_and_run;

    fn parse_and_run<T>(src: &str) -> anyhow::Result<T> {
        let (_, ast_and_type_table, errors) = parse_file(src);

        if !errors.is_empty() {
            return Err(anyhow::anyhow!("Found errors parsing file"));
        }

        let (ast, type_table) = if let Some(x) = ast_and_type_table {
            x
        } else {
            return Err(anyhow::anyhow!("Could not parse Ast"));
        };

        compile_and_run::<T>(ast, &type_table, false)
    }

    #[test]
    fn main_69() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_69.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }

    #[test]
    fn main_add() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_add.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }

    #[test]
    fn main_arithmetic() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_arithmetic.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }

    #[test]
    fn main_bool() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_bool.lang");
        let res = parse_and_run::<bool>(src)?;
        assert!(res);
        Ok(())
    }

    #[test]
    fn main_if() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_if.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }

    #[test]
    fn main_call() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_call.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }

    #[test]
    fn main_var() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_var.lang");
        let res = parse_and_run::<f64>(src)?;
        assert_eq!(res, 69.0);
        Ok(())
    }
}
