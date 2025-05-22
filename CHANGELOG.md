# Revision history for libfuse3

<!-- Memo: Stop supporting older GHCs when bumping a version -->

## 0.2.1.0 -- 2025-05-22

* Fixed the benchmark to work with `unix-2.8.*`.
* Added a C wrapper function around `fuse_new`, which is now a function macro instead of an ordinary function since an unknown version of the upstream.
* Raised upper bound: `base <4.22`, allowing ghc-9.8, 9.10 and 9.12.

## 0.2.0.1 -- 2023-03-26

* Fixed the example to compile with `unix-2.8`.
* Added support for `base-4.18` (ghc-9.6).
* Dropped support for `base-4.14.2` and older.

## 0.2.0.0 -- 2022-09-10

### Breaking changes

* Fixed the type of `FileStat.blockCount` from `CBlkSize` to `CBlkCnt`.

### Other changes

* Added support for `base-4.17.0.0` (ghc-9.4).
* Added support for `unix-2.8.0.0`.
* Removed a dependency to `linux-xattr` from the example `passthrough`, replacing with a hand-written implementation (#21).

## 0.1.2.1 -- 2022-05-20

* Enabled build for Haskell Stack ([#16](https://github.com/matil019/haskell-libfuse3/pull/16), thanks to @modotte)

## 0.1.2.0 -- 2020-11-09

* Added `throwErrnoOf`, `tryErrno'` and `tryErrno_'` to `System.Libfuse3.Utils` (#5)
* Added `ExceptionHandler` and `defaultExceptionHandler` (#6)
* Fixed a bug in `resCFuseOperations` to prevent Haskell exceptions from escaping to C land (#7)
* Added `pread` and `pwrite` to `System.Libfuse3.Utils` (#8)

## 0.1.1.1 -- 2020-10-06

* Minor improvements on the documentations
* Add tests
* Correct the version numbers in `configure.ac`

## 0.1.1.0 -- 2020-08-29

* Improve the situation with signals
  * Now possible to unmount the filesystem with signals, but have to be sent twice.

## 0.1.0.0 -- 2020-08-27

* First version. Released on an unsuspecting world.
