# mscheme

[![Haskell CI](https://github.com/HiroShinke/mscheme/actions/workflows/haskell.yml/badge.svg)](https://github.com/HiroShinke/mscheme/actions/workflows/haskell.yml)

A project for scheme interpreters and compilers implemented in Haskell Language.

This project is based on Makoto Hiroi's mscheme.

Original version is published on 

* http://www.nct9.ne.jp/m_hiroi/func/scheme.html
* http://www.nct9.ne.jp/m_hiroi/func/haskell.html

Thanks to Mr. Hiroi.
These pages has lot of information interesting for me.

This repository includes the following executables: 
* mscheme   : interpreter
* mscheme2  : interpreter(CPS version)
* mschemec  : compiler and vertual machine

You can go to the top directory and 
compile and run tests with next command:

```shell
stack test
```
and run repl with:

```shell
stack exec mscheme
stack exec mscheme2
stack exec mchemec
```
