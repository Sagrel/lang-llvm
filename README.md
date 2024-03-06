## Note that this repo is no longer in use as I merged all 3 crates into the [main repo](https://github.com/Sagrel/lang)

This will be a backend for my lang that will use [LLVM](https://llvm.org/) to generate an executable

This crate depends on [lang-frontend](https://github.com/Sagrel/lang-frontend) for parsing and type inference. The only function of this crate is to do codegen using inkwell.

To use this you must have llvm 13 installed. You can follow [this tutorial](https://docs.mun-lang.org/ch04-02-building-llvm.html) to build LLVM or get a pre-built binary.
