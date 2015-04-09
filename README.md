# Sagittarius Scheme System

# What is this?
This is a free Scheme implementation, supporting R6RS and R7RS
specification.

# How to build and install?
Sagittarius uses CMake for its building infrastructure.  If you do not
have it on your platform, please install it.

 - [CMake(must be higher than 2.8.4)](http://www.cmake.org/)

## Quick build/install (for Unix like environment)

If your environment already has required libraries and just want to install
to default location, run the following commands in the directory contains
Sagittarius source code;

    % cmake .
    % make
    % make install

Following sections describes more details.

## Preparation for Unix-like environment

Sagittarius depends on
[Boehm GC](http://www.hpl.hp.com/personal/Hans_Boehm/gc/)
you need to install it with the option `--enable-threads=pthreads`.  If
your CPU is not incredibly old, you can also specify the option
`--enable-parallel-mark`.

If you are too lazy to download the archive file of GC, CMake will
download it for you.  Make sure to run the following commands from in GC
directory:

    % ./configure \
          --enable-threads=pthreads   \
          --enable-parallel-mark      \
          --enable-large-config
    % make
    % make install

Note: most of the Linux distributions already have Boehm GC in their
package management system, such as `apt-get`.  I recommend to use it as
long as Sagittarius does not bundle it.

## Building on Unix-like environment

After installing CMake, you are ready to build Sagittarius; type the
following command:

    % cmake .

Note: The above command assumes you are in the source directory.

It is possible to build Sagittarius in a directory that is not the top
source directory of the distributed package (out-of-tree building);
for example:

    % mkdir build
    % cd build
    % cmake ${path to Sagittarius' source directory}
    % make

To install Sagittarius non default location, you need to specify
`CMAKE_INSTALL_PREFIX` variable.

    % cmake . -DCMAKE_INSTALL_PREFIX=/path/to/install

On some environment, there are 64 bits runtime specific directories such
as `lib64`. To install Sagittarius runtime in the directory, then you
can specify `LIB_DIR` variable as following;

    % cmake . -DLIB_DIR=lib64

Then the runtime install directoy will be `CMAKE_INSTALL_PREFIX/LIB_DIR`.
There are also the variables to specify `bin`, `include` and `share` 
directories, and the directory for the `.pc` files;
`BIN_DIR`, `INCLUDE_DIR` `SHARE_DIR`, and `PKGCONFIG_DIR`, respectively.

Since 0.5.6, Sagittarius's REPL is renamed to `sagittarius` and legacy
`sash` is kept as a symbolic link. If you don't need the symbolic link
then you can put the `INSTALL_SYMLINK` option off as following;

    % cmake . -DINSTALL_SYMLINK=0

After installing Boehm GC, go to the directory `gc-7.2` and type
commands below and re-run `cmake`.  Make sure you delete CMakeCache.txt
to re-run `cmake` command.

    % make
    % make test

After a successful compilation (of both the binary targets and the
documentation), it is possible to install Sagittarius to the default system
location with the command:

    % make install

After installation, you might need to run `ldconfig` to run Sagittarius
properly.

Note: For some reason, you might want to build a 32-bit runtime on a
64-bit platform.  The following command can be used for this purpose;

    % cmake . \
        -DCMAKE_CXX_COMPILER=${your 32 bit C++ compiler} \
        -DCMAKE_C_COMPILER={your 32 bit C compiler}

Make sure you have all the required 32-bit executables and libraries.

## Building on Mac OS X
See the section above (Building on Unix-like environment), and read the
following.

Building on Mac OS X is slightly different from other Unix-like environments.

Only with Homebrew is tested, so the following instruction is based on
Homebrew use.

Firstly, Install libffi, CMake, and Boehm GC.

    $ brew install libffi cmake bdw-gc

Secondly, extract Sagittarius Scheme source code, and change current directory
to there.

    $ cd /path/to/sagittarius-x.y.z

Thirdly, run cmake as follows.

    $ cmake . \
        -DCMAKE_SYSTEM_PROCESSOR=x86_64 -DCMAKE_SYSTEM_NAME=darwin \
        -DCMAKE_INSTALL_PREFIX=/path/to/install \
        -DCMAKE_FIND_FRAMEWORK=LAST \
        -Wno-dev \
        -DFFI_LIBRARY_DIR=/usr/local/Cellar/libffi/3.0.13/lib

After running cmake, run make same as other Unix environments. If `pkg-config`
can find `libffi` then `FFI_LIBRARY_DIR` won't be needed.

NOTE: some Mac OS X environment may not be able to find `ar` command
because `/usr/bin/gcc` is identical as `/usr/bin/clang`. In that case,
export `CC` and `CXX` environment variable with proper GCC and G++
command path respectively so that CMake can find the command.

## Build on Windows (non Cygwin environment)
On Windows, you need to create an installer and Sagittarius is using
innosetup for it.  Please install it.
 - [Inno Setup](http://www.jrsoftware.org/)

You need to install MSVC preferably Visual Studio 2010 or higher.  And
if you use cmake-gui, it will be much easier.  Run `Visual Studio
Command Prompt` and go to the directory which Sagittarius source codes
are expanded.

Note: I usually use `cmake-gui` to configure NMakefile, so the following
commands might not work properly.  Make sure your configuration enables
multithreading.  (Parallel mark can be optional).

    % cmake .

The final commands are almost the same as in Unix-like environments.

    % nmake
    % nmake test

After these commands, you move to the `win/` directory and double click
the file `innosetup.iss`.  Go to [Build] - [Compile], then it will
create the installer.  For more detail, please see Inno Setup's
document.

# How to develop it?

See HACKING file.
