# Version History

## 1.2 (July 28, 2020)

  - New `Settings` type with the ability to set a preferred ISO 639-1
    language code.  Functions that used to take a `Key` now take a
    `Settings` value instead.  (#5)

  - Removed the `Binary` instance for `Configuration` since the Aeson
    instances can be used to serialize to binary.

  - Reformatted all code with Ormolu and refactored some bits to
    remove unnecessary dependencies.

  - Minor updates for dependency bounds:

    - 1.2.0.1 (October 27, 2020)
    - 1.2.1 (June 2, 2021)
    - 1.2.2 (July 13, 2022) (Thanks to ragreener1)

## 1.1.5.2 (April 15, 2019)

  - Update version of `http-client`

    - Builds with `http-client` version 0.5.13.1 and 0.6.2

  - Updated the `default.nix` file to select the right version of `http-client`

## 1.1.5.1 (April 15, 2019)

  - Update dependency versions

  - Builds on NixOS 18.09, 19.03, and unstable

## 1.1.5.0 (October 09, 2018)

  - Update dependency versions

## 1.1.4.0 (March 20, 2018)

  - Update dependency versions

  - Replace `Control.Monad.Trans.Either` with `ExceptT`

## 1.1.3.0 (March 19, 2017)

  - Widen dependencies for LTS-7.20

  - Build with latest Hackage too!

  - Add Travis CI build status for supported versions of GHC

## 1.1.2.0 (June 9, 2016)

  - Widen dependencies for LTS-5.15

## 1.1.1.0 (July 22, 2015)

  - Widen dependencies for aeson, either, and text-binary

## 1.1.0.0 (May 22, 2015)

  - Added Ord instance for TV, Season, and Episode
  - Removed unused dependencies from build-depends
  - Changes to build with GHC 7.8.4. and 7.10.1

## 1.0.0.0 (April 5, 2015)

  - Major rewrite of the interface
  - Removed utility functions for loading API keys
  - Added types and functions for TV series information
  - Fixed bug: https://github.com/pjones/themoviedb/issues/1
  - Fixed bug: https://github.com/pjones/themoviedb/issues/2

## 0.1.0.1 (December 17, 2012)

  - Small internal changes to compile with GHC 7.6.1.
  - Also compiles with GHC 7.4.2 and HP 2012.4.0.0.

## 0.1.0.0 (December 14, 2012)

  - Initial release.
