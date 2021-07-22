# mscheme

[![Haskell CI](https://github.com/HiroShinke/mscheme/actions/workflows/haskell.yml/badge.svg)](https://github.com/HiroShinke/mscheme/actions/workflows/haskell.yml)

A project for scheme interpreters and compilers implemented in Haskell Language.

This project is based on Makoto Hiroi's mscheme.

Original version is published on 

* http://www.nct9.ne.jp/m_hiroi/func/scheme.html
* http://www.nct9.ne.jp/m_hiroi/func/haskell.html

Thanks to Mr. Hiroi.
These pages has lot of interesting things for me.

This repository includes the following executables: 
* mscheme   : interpreter
* mscheme2  : interpreter(CPS version)
* mschemec  : compiler and vertual machine
* mschemec2 : compiler and vertual machine for mutable cells
* mschemellvm : compiler for LLVM IR code generation (experimental)

You can go to the top directory and 
compile and run tests with next command:

```shell
stack test
```
and run repl with:

```shell
stack exec mscheme
stack exec mscheme2
stack exec mschemec
stack exec mschemec2
```

## mschemellvm

For usage of mschemellvm, see test/testllvm.sh. 
You should have LLVM installed.

```shell
stack exec mschemellvm testllvm.sc
llc testllvm.ll 
gcc -o testllvm testllvm.s
./testllvm
```

There are many restrictions in current version (and maybe also in later versions)
- only 32bit integer variable is supported.
- only +,-,*,/,=,!=,<,>,showInt,showStr primitive functions are supported.
- (define xx (lambda (args ...) expressions...)) style user function definition is supported.

