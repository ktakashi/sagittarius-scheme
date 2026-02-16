AGENTS instructions
===================

Sagittarius Scheme is a Scheme implementation supporting R6RS and R7RS specifications.
The runtime is written in C (requires C11).

Repository layout
-----------------

### Core directories

- `src/`: C source files for the Scheme runtime
  - `src/os/{os}/`: OS-dependent C source files (darwin, linux, win32, etc.)
  - `src/sagittarius/`: Header files
  - `*.stub`: Stub files that generate C code for Scheme-C bindings
- `boot/`: Bootstrap code (compiler and core libraries)
- `lib/`: Core Scheme library directory (rnrs, core, clos, compat)
- `sitelib/`: Utility Scheme libraries (srfi, rfc, crypto, etc.)
- `ext/`: Extension libraries with native C bindings
  - `threads/`, `socket/`, `crypto/`, `ffi/`, `zlib/`, etc.

### Build and tooling

- `cmake/`: CMake modules and configuration templates
- `tools/scripts/`: Build scripts and code generators
  - `builtin-keywords.scm`, `builtin-symbols.scm`: Symbol generators
  - `compile-unicode.scm`, `compile-tzdatabase.scm`: Data file generators
  - `r7rs-srfi-gen.scm`: R7RS SRFI library generator

### Testing and documentation

- `test/`: Scheme tests
  - `tests/`: Individual test files organized by category
  - `r6rs-test-suite/`, `r7rs-tests/`: Standard conformance tests
- `doc/`: Documentation in scribble and markdown formats

How to build
------------

### Prerequisites

- CMake (minimum version 3.12)
- C compiler supporting C11
- Latest release of Sagittarius installed (for code generation)
- Dependencies: Boehm GC, zlib, libffi, OpenSSL

### Build steps

#### For POSIX (Linux, macOS, BSD)

```shell
# 1. Generate code (requires existing sagittarius binary)
./dist.sh gen

# 2. Configure with CMake
cmake .

# 3. Build
make
```

#### For Windows

```cmd
:: 1. Generate code
dist.bat gen

:: 2. Configure with CMake (using Ninja)
cmake . -G Ninja

:: 3. Build
ninja
```

### Out-of-tree build (recommended)

```shell
mkdir build && cd build
cmake ..
make
```

### Using a custom Sagittarius for code generation

```shell
env SASH=/path/to/sagittarius ./dist.sh gen
```

### Key CMake options

- `-DCMAKE_INSTALL_PREFIX=/path`: Installation prefix
- `-DLIB_DIR=lib64`: Library directory name
- `-DINSTALL_SYMLINK=OFF`: Disable sash symlink
- `-DBUILD_TYPE=Debug`: Build type (default: RelWithDebInfo)

Running the interpreter
-----------------------

After building, the executable is at `./build/sagittarius`.

### Common options

```
sagittarius [options] [file]

-v, --version           Print version and exit
-h, --help              Print help and exit
-i, --interactive       Force interactive mode (REPL)
-L<path>                Add to load path (head)
-A<path>                Add to load path (tail)
-D<path>                Add to dynamic load path
-e<expr>                Evaluate expression before script
-r6, -r7                Run in strict R6RS/R7RS mode
-d, --disable-cache     Disable compiled cache
-c, --clean-cache       Clean compiled cache
-n, --no-main           Don't call main procedure
```

### Running test scripts

```shell
# Run a script with custom library paths
./build/sagittarius -L lib -L sitelib script.scm

# Interactive REPL
./build/sagittarius -i

# Evaluate expression
./build/sagittarius -e '(display "hello")'
```

Run tests
---------

Tests use CTest framework.

```shell
# Run all tests
ctest

# Run tests matching pattern
ctest -R {pattern}

# Run with verbose output
ctest -V

# Run specific test
ctest -R "srfi"
```

Code generation
---------------

The `dist.sh gen` command runs these generators:

1. `stub` - Generate C files from `.stub` files (Scheme-C bindings)
2. `precomp` - Compile core Scheme libraries to C
3. `srfi` - Generate R7RS-style SRFI library wrappers
4. `tz` - Generate timezone database
5. `unicode` - Generate Unicode codepoint tables
6. `html` - Generate HTML entity tables

Individual generators can be run separately:
```shell
./dist.sh stub     # Only stub files
./dist.sh precomp  # Only precompiled libraries
```

Key source patterns
-------------------

- `lib_*.c` in `src/`: Generated from `lib_*.stub` or compiled Scheme
- `*.stub` files: Define C-Scheme bindings using a macro DSL
- Libraries follow R6RS/R7RS naming: `(library name)` maps to `library/name.scm`

Environment variables
---------------------

- `SASH`: Path to Sagittarius executable for code generation
- `SAGITTARIUS_LOADPATH`: Additional library load paths (`:` separated)
- `SAGITTARIUS_DYN_LOADPATH`: Additional dynamic library paths

Review checklist
----------------

- All tests executed by `ctest` must pass
- Run `./dist.sh gen` if stub or precompiled library files were modified
- Rebuild after modifying C source files
