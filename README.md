# Sagittarius Scheme System

# What is this?
This is a free Scheme implementation, mostly supporting the R6RS
specification.

The official site is hosted on [Google Code](http://code.google.com/p/sagittarius-scheme).

The license is same as MIT license. See a Copyright file.

# How to build and install?
Sagittarius uses CMake for its building infrastructure.  If you do not
have it on your platform, please install it.

 - [CMake(must be higher than 2.8.4)](http://www.cmake.org/)

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

After installing Boehm GC yet, go to the directory `gc-7.2` and type
commands below and re-run `cmake`.  Make sure you delete CMakeCache.txt
to re-run `cmake` command.

    % make
    % make test
    % make doc

The command `make doc` creates HTML documentation in the directory
`doc/`.

After a successful compilation (of both the binary targets and the
documentation), it is possible to install Sagittarius to the default system
location with the command:

    % make install

or to a temporary location with the command:

    % make install DESTDIR=/path/to/tmpdir

Note: `make doc` command must be run before `make install` command, otherwise
installation process will fail

After installation, you might need to run `ldconfig` to run Sagittarius
properly.

Note: For some reason, you might want to build a 32-bit runtime on a
64-bit platform.  The following command can be used for this purpose;

    % cmake . \
        -DCMAKE_CXX_COMPILER=${your 32 bit C++ compiler} \
        -DCMAKE_C_COMPILER={your 32 bit C compiler}

Make sure you have all the required 32-bit executables and libraries.

## Building on Mac OS X
See the section above (Building on Unix-like environment), and read the following.

Building on Mac OS X is slightly different from other Unix-like environments.

Only with Homebrew is tested, so the following instruction is based on Homebrew use.

Firstly, Install libffi, CMake, and Boehm GC.

    $ brew install libffi cmake bdw-gc

Sedondly, extract Sagittarius Scheme source code, and change current directory to there.

    $ cd /path/to/sagittarius-x.y.z

Thirdly, run cmake as follows.

    $ cmake . \
        -DCMAKE_SYSTEM_PROCESSOR=x86_64 -DCMAKE_SYSTEM_NAME=darwin \
        -DINSTALL_PREFIX=/path/to/install \
        -DCMAKE_BUILD_TYPE=None \
        -DCMAKE_FIND_FRAMEWORK=LAST \
        -Wno-dev \
        -DFFI_LIBRARY_DIR=/usr/local/Cellar/libffi/3.0.13/lib

After running cmake, run make same as other Unix environments.

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
    % nmake doc

After these commands, you move to the `win/` directory and double click
the file `innosetup.iss`.  Go to [Build] - [Compile], then it will
create the installer.  For more detail, please see Inno Setup's
document.

# How to develop it?

See HACKING file.
