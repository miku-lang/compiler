# Miku Compiler

A compiler for [Miku lang](https://github.com/miku-lang). For now it generates a .c source file that you can compile with any C compiler.

```console
$ miku-lang HelloMiku.miku -o HelloMiku.c
$ gcc HelloMiku.c -o miku
$ ./miku
```
