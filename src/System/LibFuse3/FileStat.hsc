{-# LANGUAGE RecordWildCards #-}
-- | @struct stat@ in Haskell.
module System.LibFuse3.FileStat where

#include <sys/stat.h>

import Foreign (Storable, alloca, castPtr, peek, peekByteOff, pokeByteOff)
import Foreign.C (throwErrnoIfMinus1Retry_)
import System.Clock (TimeSpec)
import System.Posix.Error (throwErrnoPathIfMinus1Retry_)
import System.Posix.Internals (c_fstat, lstat, withFilePath)
import System.Posix.Types
  ( CBlkCnt
  , DeviceID
  , Fd(Fd)
  , FileID
  , FileMode
  , FileOffset
  , GroupID
  , LinkCount
  , UserID
  )

import qualified Foreign

-- | A file status a.k.a. metadata.
--
-- The differences from `System.Posix.Files.FileStatus` are:
--
--   - Is a record type with a `Storable` instance.
--
--   - Has an extra field `blockCount`.
--
--       - An equivalent accessor @fileBlocks@ was added in unix-2.8.0.0, but it is a @Maybe@.
--
--   - Provides an exact representation (`TimeSpec`) of the time fields without converting to `Date.Time.Clock.POSIX.POSIXTime`.
--
--       - This assumes that the @struct stat@ has @st_atim@, @st_mtim@ and @st_ctim@ fields.
--         On Linux this requires Linux >= 2.6.
--
-- @Ptr FileStat@ can be cast to @Ptr `System.Posix.Internals.CStat`@ and vice versa.
--
-- Use `defaultFileStat` and modify its fields you are interested in.
--
-- The @st_ino@ field is ignored unless the @use_ino@ mount option is given.
--
-- The @st_dev@ and @st_blksize@ fields are ignored by libfuse, so not provided.
data FileStat = FileStat
  { -- | Inode number. @st_ino@
    fileID :: FileID
  , -- | File type and mode. @st_mode@
    fileMode :: FileMode
  , -- | Number of hard links. @st_nlink@
    linkCount :: LinkCount
  , -- | User ID of owner. @st_uid@
    fileOwner :: UserID
  , -- | Group ID of owner. @st_gid@
    fileGroup :: GroupID
  , -- | Device ID (if special file). @st_rdev@
    specialDeviceID :: DeviceID
  , -- | Total size, in bytes. @st_size@
    fileSize :: FileOffset
  , -- | Number of 512B blocks allocated. @st_blocks@
    blockCount :: CBlkCnt
  -- these assumes Linux >= 2.6
  , -- | Time of last access. @st_atim@
    accessTimeHiRes :: TimeSpec
  , -- | Time of last modification. @st_mtim@
    modificationTimeHiRes :: TimeSpec
  , -- | Time of last status change. @st_ctim@
    statusChangeTimeHiRes :: TimeSpec
  }
  deriving (Eq, Show)

-- | Targets @struct stat@.
instance Storable FileStat where
  sizeOf _ = #size struct stat

  alignment _ = #alignment struct stat

  peek ptr = do
    fileID     <- (#peek struct stat, st_ino) ptr
    fileMode   <- (#peek struct stat, st_mode) ptr
    linkCount  <- (#peek struct stat, st_nlink) ptr
    fileOwner  <- (#peek struct stat, st_uid) ptr
    fileGroup  <- (#peek struct stat, st_gid) ptr
    specialDeviceID <- (#peek struct stat, st_rdev) ptr
    fileSize   <- (#peek struct stat, st_size) ptr
    blockCount <- (#peek struct stat, st_blocks) ptr
    accessTimeHiRes       <- (#peek struct stat, st_atim) ptr
    modificationTimeHiRes <- (#peek struct stat, st_mtim) ptr
    statusChangeTimeHiRes <- (#peek struct stat, st_ctim) ptr
    pure FileStat{..}

  poke ptr FileStat{..} = do
    (#poke struct stat, st_ino)    ptr fileID
    (#poke struct stat, st_mode)   ptr fileMode
    (#poke struct stat, st_nlink)  ptr linkCount
    (#poke struct stat, st_uid)    ptr fileOwner
    (#poke struct stat, st_gid)    ptr fileGroup
    (#poke struct stat, st_rdev)   ptr specialDeviceID
    (#poke struct stat, st_size)   ptr fileSize
    (#poke struct stat, st_blocks) ptr blockCount
    (#poke struct stat, st_atim)   ptr accessTimeHiRes
    (#poke struct stat, st_mtim)   ptr modificationTimeHiRes
    (#poke struct stat, st_ctim)   ptr statusChangeTimeHiRes

-- | The default value of `FileStat`.
--
-- The Haskell Equivalent of zero-setting C code @{ struct stat st; memset(&st, 0, sizeof(struct stat)); }@.
defaultFileStat :: FileStat
defaultFileStat = FileStat 0 0 0 0 0 0 0 0 0 0 0

-- | Reads a file status of a given file.
--
-- Calls @lstat@.
getFileStat :: FilePath -> IO FileStat
getFileStat path =
  alloca $ \buf ->
    withFilePath path $ \cpath -> do
      throwErrnoPathIfMinus1Retry_ "getFileStat" path (lstat cpath (castPtr buf))
      peek buf

-- | Reads a file status of a given file.
--
-- Calls @fstat@.
getFileStatFd :: Fd -> IO FileStat
getFileStatFd (Fd fd) =
  alloca $ \buf -> do
    throwErrnoIfMinus1Retry_ "getFileStatFd" (c_fstat fd (castPtr buf))
    peek buf
