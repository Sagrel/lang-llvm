use std::{path::Path};

use anyhow::Ok;
use compiler::Compiler;
use inkwell::{
    context::Context,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
    },
    OptimizationLevel,
};
use lang_frontend::{
    ast::{Anotated, Ast},
    types::Type,
};

mod compiler;

pub fn compile_to_file(
    ast: Vec<Anotated<Ast>>,
    type_table: &[Type],
    path: &Path
) -> anyhow::Result<()> {
    let context = Context::create();

    let compiler = Compiler::new(&context, type_table);

    let module = compiler.compile_module(ast)?;

    module.verify().unwrap();

    Target::initialize_x86(&InitializationConfig::default());
    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-windows-msvc"),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .unwrap();

    target_machine
        .write_to_file(&module, FileType::Object, path)
        .unwrap();

    Ok(())
}

pub fn compile_and_jit<T>(
    ast: Vec<Anotated<Ast>>,
    type_table: &[Type],
    print_module: bool,
) -> anyhow::Result<T> {
    let context = Context::create();

    let compiler = Compiler::new(&context, type_table);

    let module = compiler.compile_module(ast)?;

    module.verify().unwrap();

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

    use crate::compile_and_jit;

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

        compile_and_jit::<T>(ast, &type_table, false)
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
    fn main_call_inmediate() -> anyhow::Result<()> {
        let src = include_str!("../examples/main_call_inmediate.lang");
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
