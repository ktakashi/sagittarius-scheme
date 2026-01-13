# Sagittarius Scheme System

[![Sagittarius CI](https://github.com/ktakashi/sagittarius-scheme/actions/workflows/ci.yml/badge.svg)](https://github.com/ktakashi/sagittarius-scheme/actions/workflows/ci.yml)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/ktakashi/sagittarius-scheme/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/ktakashi/sagittarius-scheme/tree/master)

This is a Scheme implementation, supporting R6RS and R7RS specification.

# How to build and install?

Sagittarius uses CMake for its building infrastructure.  If you do not
have it on your platform, please install it.

 - [CMake](http://www.cmake.org/)

The minimum required version of CMake is 3.12. The older version may
work but not guaranteed.

## Quick build/install (for Unix like environment)

If your environment already has required libraries and just want to
install to default location, run the following commands in the
directory where all distributed files are expanded (c.f. By default
it'd be `sagittarius-X.X.X`, `X.X.X` is the version you downloaded):

```shell
cmake .
make
make install
```

Following sections describes more details.

## Preparation for Unix-like environment

Sagittarius depends on the following libraries.

 - [Boehm GC](http://www.hpl.hp.com/personal/Hans_Boehm/gc/)
 - [zlib](http://www.zlib.net/)
 - [libffi](https://sourceware.org/libffi/)
 - [OpenSSL](https://www.openssl.org/)

If you are using Linux which supports `apt`, then you can simply
execute the following command:

```shell
apt install libgc-dev, zlib1g-dev libffi-dev libssl-dev libpam0g-dev
```

### Manual installation of Boehm GC

For Boehm GC, you need to install it with the option
`--enable-threads=pthreads`.  If your CPU is not incredibly old, you
can also specify the option `--enable-parallel-mark`.

If you are too lazy to download the archive file of GC, CMake will
download it for you.  Make sure to run the following commands from in
GC directory:

```shell
./configure \
    --enable-threads=pthreads   \
    --enable-parallel-mark      \
    --enable-large-config
make
make install
```

Note: most of the Linux distributions already have Boehm GC in their
package management system, such as `apt-get`.  I recommend to use it
for security reason.

## Building on Unix-like environment

After installing CMake and dependent libraries, you are ready to build
Sagittarius; type the following command:

```shell
cmake .
```

Note: The above command assumes you are in the source directory.

It is possible to build Sagittarius in a directory that is not the top
source directory of the distributed package (out-of-tree building);
for example:

```shell
mkdir build
cd build
cmake ${path to Sagittarius' source directory}
make
```

To run the tests, specify `test` target.

```shell
make test
```

Or, alternatively, you can also use `ctest`. This is convenient to
test individual tests.

To install Sagittarius non default location, you need to specify
`CMAKE_INSTALL_PREFIX` variable.

```shell
cmake . -DCMAKE_INSTALL_PREFIX=/path/to/install
```

On some environment, there are 64 bits runtime specific directories
such as `lib64`. To install Sagittarius runtime in the directory, then
you can specify `LIB_DIR` variable as the following:

```shell
cmake . -DLIB_DIR=lib64
```

Then the runtime install directoy will be
`CMAKE_INSTALL_PREFIX/LIB_DIR`.  There are also the variables to
specify `bin`, `include` and `share` directories, and the directory
for the `.pc` files; `BIN_DIR`, `INCLUDE_DIR` `SHARE_DIR`, and
`PKGCONFIG_DIR`, respectively.

Since 0.5.6, Sagittarius's REPL is renamed to `sagittarius` and legacy
`sash` is kept as a symbolic link. If you don't need the symbolic link
then you can put the `INSTALL_SYMLINK` option off as the following:

```shell
cmake . -DINSTALL_SYMLINK=0
```

After a successful compilation, it is possible to install Sagittarius
to the location specified by `CMAKE_INSTALL_PREFIX` or default system
location if it's not specified with the command:

```shell
make install
```

After installation, you might need to run `ldconfig` to run
Sagittarius properly.

Note: For some reason, you might want to build a 32-bit runtime on a
64-bit platform.  The following command can be used for this purpose;

```shell
cmake . \
   -DCMAKE_CXX_COMPILER=${your 32 bit C++ compiler} \
   -DCMAKE_C_COMPILER={your 32 bit C compiler}
```

Make sure you have all the required 32-bit executables and libraries.

## Building on Mac OS X

Only with Homebrew is tested. A user can install sagittarius directly with
homebrew, via

```shell
brew install sagittarius-scheme
```

Alternately, the user can install the following dependencies and then make
sagittarius locally.

Installing libffi, CMake, Boehm GC and OpenSSL.

```shell
brew install libffi cmake bdw-gc openssl
```

After installing dependent libraries, the rest of the process are the same
as Unix-like environment.

If `cmake` can't find `libffi`, then you can specify the location via
`FFI_LIBRARY_DIR` option like the following.

```shell
cmake . -DFFI_LIBRARY_DIR=/opt/homebrew/Cellar/libffi/3.4.6/lib
```

Note: some Mac OS X environment may not be able to find `ar` command
because `/usr/bin/gcc` is identical as `/usr/bin/clang`. In that case,
export `CC` and `CXX` environment variable with proper GCC and G++
command path respectively so that CMake can find the command.

## Building on FreeBSD

FreeBSD has multiple type of Boehm GC system libraries; `gc`,
`gc-threaded` and `gc-redirect`. Sagittarius requires threaded runtime
to make thread library works properly. The building process checks if
`gc` has `GC_get_parallel()` function and if it doesn't then tries to
use `gc-threaded` library. Please make sure your system has
`gc-threaded` or `gc` built with multi thread option.

NB: If you install `gc` without build option, then default is without
thread support. In such a case, you need to install `gc-threaded` as
well.

The command below installs the requried dependencies.

```sh
pkg install curl libffi boehm-gc-threaded cmake openssl bash
```

## Building on OpenBSD

For OpenBSD, you can use `pkg_add` command to install dependencies.
You can use the command below to install the required components.

```sh
pkg_add -IUv curl libffi boehm-gc cmake bash gmake
```

NOTE: the command is expecting to be fresh environment.

## Building on Windows (non Cygwin environment)

On Windows, you need to create an installer and Sagittarius is using
innosetup for it. Please install it.

 - [Inno Setup](http://www.jrsoftware.org/)

You need to install MSVC preferably Visual Studio 2010 or higher. Run
`Visual Studio Command Prompt` and go to the directory which Sagittarius
source codes are expanded.

### For NMake

```bat
cmake . -G"NMake Makefiles"
nmake
nmake test
```

### For Ninja

```bat
cmake . -G"Ninja"
ninja
```

For Ninja, you can build with multiple processor by specifying `-j` option.
For example:
```bat
rem This build Sagittarius with 8 processes
ninja -j8
```

After these commands, you move to the `win/` directory and double
click the file `innosetup.iss`.  Go to [Build] - [Compile], then it
will create the installer.  For more detail, please see Inno Setup's
document.

### Other Windows build tools

Experimentally, we support other build tools. Below are the supported ones:

- MSBuild
  ```bat
  cmake .
  msbuild /p:Configuration=Release Sagittarius.sln
  ```


### Supporting platform

| OS      | x86 | amd64 | ARM64 | ARM | ppc64le |
|---------|-----|-------|-------|-----|---------|
| Linux   |     | ✅✔️ | ☑️✔️  | ✔️  | ✔️     |
| macOS   |     |       | ✅    |     |         |
| FreeBSD |     | ✅    | ☑️    |     |         |
| OpenBSD |     | ✅    | ☑️    |     |         |
| Windows | ✅  | ✅    | ☑️    |     |         |

- ✅ Tested with CI
- ☑️ Tested locally
- ✔️ Docker image

## Running test

The tests are executed via `CTest`. In case you want to have verbose output,
you can use `ctest` command. For example, the below command shows the output
of the test execution when the test failed.

```shell
ctest --output-on-failure
```

For more options, please refer the official document of the
[`CTest`](https://cmake.org/cmake/help/latest/manual/ctest.1.html)

# Forums and bug reporting

If you have any question, please ask on the following Google group:

- [sagittarius-scheme](https://groups.google.com/forum/#!forum/sagittarius-scheme)

If you find a bug, please report it on the project bug tracking board:

- [Sagittarius Issues](https://github.com/ktakashi/sagittarius-scheme/issues)

Or you can report it on Google group.

# How to develop it?

See [HACKING.md](HACKING.md) file.

<!-- end of file
Local Variables:
mode: markdown
fill-column: 75
End:
-->
