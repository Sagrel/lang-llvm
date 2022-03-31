extern crate lang_frontend;

extern crate inkwell;

use std::{env, fs, path::Path};

use lang_llvm::{compile_and_jit, compile_to_file};

use lang_frontend::parse_file;


pub fn main() -> anyhow::Result<()> {
    let path = if let Some(path) = env::args().nth(1) {
        path
    } else {
        "examples/add_args.lang".to_owned()
    };

    let src = fs::read_to_string(path)?;
    parse_file(&src);

    let (_, ast_and_type_table, errors) = parse_file(&src);

    if !errors.is_empty() {
        return Err(anyhow::anyhow!("Found errors parsing file"));
    }

    let (ast, type_table) = if let Some(x) = ast_and_type_table {
        x
    } else {
        return Err(anyhow::anyhow!("Could not parse Ast"));
    };

    for (node,_,_) in &ast {
        println!("{}", node);
    }

    //compile_to_file(ast, &type_table, Path::new("./examples/executables/res.o"))?;
     
    println!(
        "Main returned: {}",
        compile_and_jit::<f64>(ast, &type_table, true)?
    );
    
    Ok(())
}
