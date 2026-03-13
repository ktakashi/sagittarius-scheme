---
name: document
description: Guide for executing Sagittarius tests. Use this when asked to execute tests.
---

Using CTest
===========

Sagittarius' tests can be executed via `ctest` command

Execute full test
-----------------

Use the command below to execute full test

```shell
ctest --output-on-failure -j 6
```

The value of the `-j` option can be changed. It's better not to exceed the
number of CPU core.

Execute individual tests
------------------------

Executing individual tests has multiple options.

### Using -R option

Using `-R` option filters the tests by name.

Example command
```shell
ctest --output-on-failure -R sagittarius
```

This executes the tests whose name contain `sagittarius`

### Using -I option

Using `-I` option executes the specific range of the test

Example: execute tests for even numbers until 50
```shell
ctest --output-on-failure -I 2,50,2
```

### Check the test name and number

Use this command to check the name and test number

```shell
ctest -N
```

Using Sagittarius directly
--------------------------

You can run the test by using `sagittarius` command as well.

Basic command
-------------

```shell
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild test/runner.scm {test-file}
```

The test files are located in `test/` directly.

The command can be used to execute other Scheme files, if you want to check
outside of the tests.

Example: run continuation tests
```shell
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild test/runner.scm \
 test/tests/sagittarius/continuations.scm
```

Clean up cache file
===================

When the code change happens, you might need to clear the cache file.
You can use the command below to clear the cache.

```shell
./build/sagittarius -c -e '(exit)'
```

**IMPORTANT**
Make sure you use `-e` option with `(exit)` expression to exit the
application, otherwise the application goes into REPL, and waits for
the input.

Inline execution
================

You can execute a Scheme script directly by passing expressions via
`-e` command.

Example: say hello
```shell
./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild \
 -e '(display "hello")(newline)(exit)'
```

**IMPORTANT**
The same rule as the cleanup cache file command  is applied here.
Make sure the last expression is `(exit)`.

