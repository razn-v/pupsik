# pupsik

Pupsik is a statically typed programming language.
It is not meant to be used in serious projects but rather to show how to make a compiled language using LLVM and Rust. Indeed, the language is lacking multiple features and would not be appropriate to be used in daily projects. The source code is documented so that you can always refer to it if you want to make your own language.

# Prerequisites

## On Linux
* clang package

## On Windows
* [Visual Studio](https://visualstudio.microsoft.com/fr/) with the option "Desktop development with C++"
* [clang 12.0.1](https://github.com/llvm/llvm-project/releases/tag/llvmorg-12.0.1). Make sure the `bin` directory of the installation directory is in your `PATH` environment variable

# Code examples

## Hello world

```
fnc main: void -> int64 {
    @printf("Hello, world!\n");
    return 0;
}
```

## Fibonacci

```
fnc fibo: int64 n -> int64 {
    if n <= 1 {
        return n;
    } else {
        return fibo(n-1) + fibo(n-2);
    };
}

fnc main: void -> int64 {
    let result = fibo(25);
    @printf("%d\n", result);
    return 0;
}
```
