---
name: compiler-development
description: Guide for developing Sagittarius compiler. Use this when you need to develop or optimize the compiler.
---
# Sagittarius Scheme Compiler Development Guide
This guide provides an overview of the Sagittarius Scheme compiler architecture, optimization strategies, and best practices for development.

## Compiler Architecture
The Sagittarius compiler is written in Scheme, and consists of several phases:

1. **Expansion**: Macro expansion and syntactic transformations
2. **Optimization**: Code transformations for performance improvements
3. **Code Generation**: Emitting C code from the optimized intermediate representation

# File location

The compiler source code is located in the `boot/` directory
- `boot/compiler.scm` - Main compiler entry point
- `boot/lib/pass*.scm` - Optimization passes
- `boot/lib/compiler-util.scm` - Utility library for compiler
- `boot/lib/iform.scm` - Intermediate form definitions
- `boot/lib/smatch.scm` - Pattern matching utilities, use `include` syntax to include in the compiler source files

# Building the compiler

The compiler is built by the compiler from the host Sagittarius by running the command below:

For POSIX
```shell
./dist.sh precomp
```

For Windows
```bat
dist.bat precomp
```

This will generate the C code for the compiler in `src/` directory. To execute the compiler
you need to build the Sagittarius first by running the command below:

For POSIX
```shell
make sash -j$(nproc)
```

For Windows (Ninja)
```bat
ninja sash -j %NUMBER_OF_PROCESSORS%
```

# Analysis helper

To analyze the optimisation result, you can use the `compile-p*` procedure to print the intermediate form of the code after each pass. The below example shows how to use it.

```scheme
(import (sagittarius compiler))

(compile-p1 '((lambda (x) (+ x 1)) 5)) ;; shows result of pass1
(compile-p2 '((lambda (x) (+ x 1)) 5)) ;; shows result of pass2
(compile-p3 '((lambda (x) (+ x 1)) 5)) ;; shows result of pass3
(compile-p4 '((lambda (x) (+ x 1)) 5)) ;; shows result of pass4
(compile-p5 '((lambda (x) (+ x 1)) 5)) ;; shows result of pass5 (generated VM instructions)
```

You can run the script with the command below:

POSIX:
```shell
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild ${file}
```

Windows:
```bat
.\build\sash.exe -Llib -Lsitelib -Lext/* -Dbuild %file%
```
