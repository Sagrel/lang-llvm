extern crate lang_frontend;

extern crate inkwell;

use std::{env, fs};

use lang_llvm::compile_and_run;

use lang_frontend::parse_file;


pub fn main() -> anyhow::Result<()> {
    let path = if let Some(path) = env::args().nth(1) {
        path
    } else {
        "./examples/simple.lang".to_owned()
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

    compile_and_run(ast, &type_table)
}
