Changes
=======

* Drop support for GHC 7.

* Tested with GHC 8.0 - 9.12.2.

Version 3.3.2 (18 Jul 2024)
-------------

* Make `--accept` work (closes [#24](https://github.com/phile314/tasty-silver/issues/24)).
  With this flag the golden value will automatically get updated.

  It is possible to run `--accept` together with `--interactive`
  although this combination does not make much sense.
  (This will show the diff interactively but then just update the golden value.)

  The fix of `--accept` unifies the code for interactive and non-interactive test runs
  and might have introduced changes in the test output formatting.

* Tested with GHC 8.0 - 9.10.1.

Version 3.3.1.3 (20 Oct 2023)
---------------

_Dedicated to Anne-Christine. Happy birthday!_

* Fix test reporting when built with `tasty-1.5`
  (regression in 3.3.1.2, see [#41](https://github.com/phile314/tasty-silver/issues/41)).
* `filterWithRedex` is now the identity if no inclusion/exclusion regexes are given in the options.
* Drop support for GHC 7.4 for the sake of `LambdaCase`.
* Tested with GHC 8.0 - 9.8.1.

Version 3.3.1.2 (10 Sep 2023)
---------------

* Fix compilation with `tasty-1.5`
* Tested with GHC 7.4 - 9.8.0

Version 3.3.1.1 (8 May 2022)
---------------

* Fix compilation with `mtl-2.3`
* Tested with GHC 7.4 - 9.2.2

Version 3.3.1 (13 Oct 2021)
-------------

* No longer treat `git diff` warnings as failure (#33)

Version 3.3 (20 Sep 2021)
-----------

* Windows portability (#16):
  - Calls to `git diff` are no longer indirected via `sh -c`.
  - When indirection via `sh -c` is used, backslashes in filenames are converted to slashes.
* Tested with GHC 7.4 - 9.2.1-RC1

Version 3.2.3 (13 Sep 2021)
-------------

* Tested with GHC 7.4 - 9.0 (fixed compilation with GHC 7.4 - 7.8)
* CI via GitHub Actions on platforms `ubuntu`, `macOS`, `windows`.

Version 3.2.2 (22 Jun 2021)
-------------

* Fix cabal warning (#27, thanks to felixonmars)

Version 3.2.1 (22 Dec 2020)
-------------

* Fix option parser (#25)

Version 3.2 (21 Dec 2020)
-----------

* Compatibility with tasty 1.4 (breaks compatibility with older versions of tasty)

Version 3.1.15 (8 Jun 2020)
--------------

* Fix missing space in git diff calls introduced in v3.1.14 (#22, thanks to croyzor)

Version 3.1.14 (8 Jun 2020)
--------------

* Fix wrong interpretation of git diff exit codes (#21, thanks to croyzor)

Version 3.1.13 (12 Jul 2019)
--------------

* Add option to disable ansi tricks (#18, thanks to L-TChen)

Version 3.1.12 (24 Sep 2018)
--------------

* Fix compilation with GHC 8.4 (thanks to asr)

Version 3.1.11 (29 Dec 2017)
--------------

* Fix compilation with GHC 8.4

Version 3.1.10 (1 Apr 2017)
--------------

* Better error handling for calls to external tools (`git diff`)

Version 3.1.9 (29 Aug 2016)
-------------

* Fix compilation with optparse-applicative 0.13.*.
* Provide character-level diff if wdiff and colordiff are available.

Version 3.1.8.1 (19 Jan 2016)
---------------

* Fix compilation with GHC 8.

Version 3.1.8 (10 Nov 2015)
-------------

* Make update function optional for test cases.

Version 3.1.7 (14 May 2015)
-------------

* Add feature to disable certain tests, still showing them in the UI
  but not running them.
* Fix a concurrency issue in the interactive test runner.

Version 3.1.6 (14 May 2015)
-------------

* Expose regex filter modules.
* Fix issue with regex filters when used together with withResource nodes.

Version 3.1.5 (12 Apr 2015)
-------------

* Add experimental --regex-include option to select tests using a regex.
  This option is highly experimental and may change in later versions!
* The --regex-include/--regex-exclude option may be given multiple times now.
  The exclusion regexes are applied first, after that all inclusion regexes.

Version 3.1.4 (12 Apr 2015)
-------------

* Add experimental --regex-exclude option to filter out tests using a regex.
  This option is highly experimental and may change in later versions!

Version 3.1.3 (6 Apr 2015)
-------------

* Use package temporary instead of temporary-rc.
* Re-add command line options for test runner which were accidentally removed.

Version 3.1.2 (6 Apr 2015)
-------------

* Add non-interactive mode to test runner, printing diffs/actual values directly to stdout.
  Useful for (travis) CI.

Version 3.1.1 (4 Apr 2015)
-------------

* Report success instead of failure if new result is accepted in interactive mode.

Version 3.1 (4 Apr 2015)
-----------

* Fixed & tested support for GHC 7.4.2 - 7.10.1
* Added missing lower bound for bytestring
* Removed upper bounds for most dependencies
* Enable travis CI builds

Version 3.0 - 3.0.2.2
---------------------

* Refactored API
* Add interactive mode

Version 2.2.2.4
---------------

* Warn when some tests threw exceptions during `--accept`
* Properly handle exceptions; don't swallow Ctrl-C

Version 2.2.2.3
---------------

Restore compatibility with older compilers

Version 2.2.2.1
---------------

Relax `Cabal` dependency

Version 2.2.2
-------------

Add `findByExtension`

Version 2.2.1.2
---------------

Catch exceptions when accepting golden tests

Version 2.2.1.1
---------------

Switch to `temporary-rc`

Version 2.2.1
-------------

* Fix a bug where the result of the comparison function would reference yet
  unread data from a semiclosed file and the file gets closed, leading to a
  runtime exception
* Export `writeBinaryFile`
* Improve the docs
* Update to work with `tasty-0.8`

Version 2.2.0.2
---------------

Update to work with `tasty-0.7`

Version 2.2.0.1
---------------

Update to work with `tasty-0.5`

Version 2.2
-----------

Migrate to ingredients

Version 2.1
-----------

Add `goldenVsStringDiff`

Version 2.0.1
-------------

Update to work with `tasty-0.2`

Version 2.0
-----------

Initial release of `tasty-golden` (derived from `test-framework-golden-1.1.x`)
