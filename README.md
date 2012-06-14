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

    % ./configure --enable-threads=pthreads --enable-parallel-mark
    % make
    % make install

NOTE: On CYGWIN, cmake does not find `gc.h` by default (at least on my
environment), you might need to give `--prefix=/usr` option to configure script.

## Build on UNIX like environment
When you installed CMake, you are ready to build Sagittarius. Type following
command.

    % cmake .

If you did not install Boehm GC yet, you need to install it before doing next
step. Go to `gc-7.1` directory and type commands above and re-run `cmake`
command. Make sure you delete CMakeCache.txt to re-run `cmake` command.

    % make
    % make test
    % make doc
    % make install

The commend `make doc` creates document in the directory `doc/`. It is a HTML
file and 'make install' does not install it on your environment. 

You might need to run `ldconfig` to run Sagittarius properly.

## Build on Windows (non Cygwin environment)
On Windows, you need to create an installer and Sagittarius is using innosetup
for it. Please install it.
 - [Inno Setup](http://www.jrsoftware.org/)

You need to install MSVC preferably Visual Studio 2010 or higher. And if you use
cmake-gui, it will be much easier. Run `Visual Studio Command Prompt` and go to
the directory which Sagittarius source codes are expanded.

Note: I usually use `cmake-gui` to configure NMakefile, so following commands
might not work properly.

    % cmake .

Before generating MakeFile, you might need to add the following line to
`gc-7.2/CMakeLists.txt` around line number 200.

    ADD_DEFINITIONS("-DGC_WIN32_THREADS")

This is because Sagittarius is using threads but some how current alpha archive
does not set this macro. 

And if you don't have zlib on your developping environment, you also need to add
the following line to `ext/zlib/zlib-{current-zlib-version}/CMakeLists.txt`.

    INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

The place does not matter as long as `cmake` can recognise it. My suggestion is
after line number 75 (if cmake downloads version 1.2.6) where it adds other
include directory.


If you are using command line `cmake`, make sure you  delete CMakeCache.txt and
re-run `cmake`. If you are using `cmake-gui`, then just press `Generate` button.

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

