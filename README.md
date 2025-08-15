# Miku Compiler

A compiler for [Miku lang](https://github.com/miku-lang). For now it generates a .c source file that you can compile with any C compiler.

```console
$ miku-lang HelloMiku.miku -o HelloMiku.c
$ gcc HelloMiku.c -o miku
$ ./miku
```

## Caveats

You can't put an track inside another track.
```rs
[[0, 1, 2], [0, 1, 2], [0, 1, 2]] // won't compile
```
