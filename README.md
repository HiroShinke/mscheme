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
ç
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
For usage of mschemellvm, see test/testllvm.sh. 

```shell
stack exec mschemellvm testllvm.sc
llc testllvm.ll 
gcc -o testllvm testllvm.s
./testllvm
```

There are many restrictions at now (and forever)
- only 32bit integer variable is supported
- no closure
- no macro
- no list
