# Revision history for libfuse3

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
