2.0 (2023-06-28)

  * Wrote a new args-example script
  * Added a note about specifying the Stackage resolver
  * Switched to modern cabal file instead of hpack
  * Moved Stackage resolver up to 16.11 (ghc 8.8.3)
  * Removed Setup.hs
  * Added example of custom error message with lookupEnv
  * Removed instructions for darcs
  * Removed later copyright date
  * Reorganized the functions in args-example.hs
  * Updated cabal file and README to reflect multiple scripts


1.8 (2021-09-05)

  * Moved copyright up to 2021
  * Added stack.yaml.lock to .gitignore
  * Replace forSystem function


1.7 (2019-08-22)

  * Added sequence expression example
  * Replaced a case statement with an if


1.6 (2018-10-03)

  * Moved copyright date up to 2018
  * Replaced out-of-fashion `fmap` with newfangled Applicative fmap
  * Changed to two-space indenting
  * Fixed a git source repository URL that was missed
  * Added some path split and dir examples
  * Removed unused TODO.md file
  * Moved stack resolver up to 12.11
  * Added example #! for runhaskell
  * Switched to hpack
  * Removed Vim swapfiles from .gitignore


1.5 (2016-11-18)

  * Changed license from BSD3 to ISC
  * Moved copyright year up to 2016
  * Switched build and execution from cabal/runhaskell to stack
  * Commented out example code that uses regexp, it imposes more deps
  * Some changes to avoid defaultTimeLocale confusion


1.4 (2015-07-04)

   * Added time-locale-compat for backwards compatibility


1.3 (2015-07-03)

   * Most of the commented examples are now live code
   * Replaced deprecated System.Cmd with System.Process
   * Updated copyright date
   * Fixed ambiguous import of defaultTimeLocale
   * Fixed some things in the cabal file
   * Added README.md file


1.2 (2014-02-28)

   * Updated cabal fields
   * Updated copyright date
   * Now using a shorter date format string
   * Updated date/time code for System.Time deprecation


1.1 (2013-09-13)

   * Added a Setup.hs to make this a valid cabalized package
   * Added new utility functions toExitCode, exitBool and forSystem
   * Added custom boringfile
  

1.0 (2013-04-15)

   * Initial release
