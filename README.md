This package provides support for «golden testing».

A golden test is an IO action that writes its result to a file.
To pass the test, this output file should be identical to the corresponding
«golden» file, which contains the correct result for the test.

Examples
--------

For a non-trivial example see [agda-compiler-tests](https://github.com/phile314/agda-compiler-tests/blob/master/test/exec/Main.hs),
which is used for testing a compiler for the Agda language.

Maintainers
-----------

[Philipp Hausmann](https://github.com/phile314) is the primary maintainer.
