This will be a backend for my lang that will use [LLVM](https://llvm.org/) to generate an executable

This crate depends on ``lang-frontend`` for parsing and type inference. The only function of this crate is to do codegen using inkwell.

To use this you must have llvm 13 installed. You can follow [this tutorial](https://docs.mun-lang.org/ch04-02-building-llvm.html) to build LLVM or get a pre-built binary.