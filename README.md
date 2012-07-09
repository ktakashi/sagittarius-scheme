# Sagittarius Scheme System

# What is this?
This is a free Scheme implementation which mostly support R6RS specification.

# How to build and install?
Sagittarius is usign CMake for its building mechanism. If you do not have it in
your platform, please install it.

 - [CMake(must be higher than 2.8.4)](http://www.cmake.org/)

## Preparation for UNIX like environment
Sagittarius depends on [Boehm GC](http://www.hpl.hp.com/personal/Hans_Boehm/gc/)
if your environment does not have it, you need to install it with
`--enable-threads=pthreads` options. If your CPU is not incredibly old, you can
also specify `--enable-parallel-mark`.

If you are lazy enough to download the archive file of GC, CMake will download
it for you. Make sure you need to go to the GC directory and type following
command;

    % ./configure --enable-threads=pthreads --enable-parallel-mark --enable-large-config
    % make
    % make install

Note: I beleive most of UNIX like environment already have Boehm GC runtime
in its software management system such as `apt-get`. I recommend to use it as
long as I don't include it in Sagittarius.

Note: On Cygwin, cmake does not find `gc.h` by default (at least on my
environment), you might need to give `--prefix=/usr` option to configure script.

## Build on UNIX like environment
When you installed CMake, you are ready to build Sagittarius. Type following
command.

    % cmake .

Note: You can also build out-of-tree using CMake default behaviour and the build
process won't contaminate the source directory at all.

If you did not install Boehm GC yet, you need to install it before doing next
step. Go to `gc-7.2` directory and type commands above and re-run `cmake`
command. Make sure you delete CMakeCache.txt to re-run `cmake` command.

    % make
    % make test
    % make doc
    % make install

The commend `make doc` creates document in the directory `doc/`. It is a HTML
file.

You might need to run `ldconfig` to run Sagittarius properly.

Note: Since 0.3.4, we also support 64 bit environment. For some reason, FFI test
might not be able to find the test runtime module and report 4 tests failed.

Note: For some reason, you might want to build 32 bit runtime on 64 bit
environment. The following command can be used for this purpose;

    % cmake . -DCMAKE_CXX_COMPILER=${your 32 bit C++ compiler} -DCMAKE_C_COMPILER={your 32 bit C compiler}

Make sure you have all required runtime with 32 bit.


## Build on Windows (non Cygwin environment)
On Windows, you need to create an installer and Sagittarius is using innosetup
for it. Please install it.
 - [Inno Setup](http://www.jrsoftware.org/)

You need to install MSVC preferably Visual Studio 2010 or higher. And if you use
cmake-gui, it will be much easier. Run `Visual Studio Command Prompt` and go to
the directory which Sagittarius source codes are expanded.

Note: I usually use `cmake-gui` to configure NMakefile, so following commands
might not work properly. Make sure your configuration enables threads option.
(parallel mark can be optional).

    % cmake .

The rest commands are almost the same as UNIX like environment.

    % nmake
    % nmake test
    % nmake doc

After these commands, you need to go to `win/` directory and double click the
file `innosetup.iss`. Go to [Build] - [Compile], then it will create the
installer. For more detail, please see Inno Setup's document.

# How to develop it?
We provide `autogen.sh` for developper and it generates boot code, VM
instrustions and generated code from stub file. For more detail, see the file.

NOTE:
If Sagittarius itself is so unstable to generate boot code, you can also use
[Gauche](http://practical-scheme.net/gauche/index.html) for it.

