# Sagittarius Scheme System

# What is this?
This is a free Scheme implementation which mostly support R6RS specification.

# How to build and install?
Sagittarius is usign CMake for its building mechanism. If you do not have it in
your platform, please install it.

 - [CMake(must be higher than 2.8.4)](http://www.cmake.org/)

## Build on Linux like environment
When you installed CMake, you are ready to build Sagittarius. Type following
command.

    % cmake .
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

Note: I usually use cmake-gui to configure NMakefile, so following commands
might not work properly.

    % cmake .
    % nmake
    % nmake test
    % nmake doc

After these commands, you need to go to `win/` directory and double click the
file `innosetup.iss`. Go to [Build] - [Compile], then it will create the
installer. For more detail, please see Inno Setup's document.


# How to develop it?
Sagittarius requires [Gauche](http://practical-scheme.net/gauche/index.html) to
develop boot code, such as compiler and builtin Scheme libraries.

We provide `autogen.sh` for developper and it generates boot code, VM
instrustions and generated code from stub file. For more detail, see the file.