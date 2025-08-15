# Miku Compiler

A compiler for [Miku lang](https://github.com/miku-lang). For now it generates a .c source file that you can compile with any C compiler.

```console
$ miku-lang HelloMiku.miku -o HelloMiku.c
$ gcc HelloMiku.c -o miku
$ ./miku
```

## Caveats

### Track of tracks

You can't put an track inside another track.
```rs
[[0, 1, 2], [0, 1, 2], [0, 1, 2]] // won't compile
```

### Native module

You can use a C library as a module if you create a file named `<name>.album` containing the definition of the functions and some typed variable (no initializer).\
Then you can `remix <name>` and use those functions as `<name>.function()`.

`<name>` should be the name of the .h file since the compiler will `#include <name.h>`, and you'll have to link `lib<name>.a` yourself.

See: examples/Rayliber.miku and examples/raylib.album
