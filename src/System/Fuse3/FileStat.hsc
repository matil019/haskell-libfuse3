{-# LANGUAGE RecordWildCards #-}
-- | @struct stat@ in Haskell.
module System.Fuse3.FileStat where

#include <sys/stat.h>

import Foreign (Storable, alloca, castPtr, peek, peekByteOff, pokeByteOff)
import System.Clock (TimeSpec)
import System.Posix.Error (throwErrnoPathIfMinus1Retry_)
import System.Posix.Internals (lstat, withFilePath)
import System.Posix.Types
  ( CBlkSize
  , DeviceID
  , FileOffset
  , FileMode
  , GroupID
  , LinkCount
  , UserID
  )

import qualified Foreign

-- Ptr FileStat === Ptr CStat
data FileStat = FileStat
  { fileMode :: FileMode -- st_mode
  , linkCount :: LinkCount -- st_nlink
  , fileOwner :: UserID -- st_uid
  , fileGroup :: GroupID -- st_gid
  , specialDeviceID :: DeviceID -- st_rdev TODO what for non-device file?
  , fileSize :: FileOffset -- st_size
  , blockCount :: CBlkSize -- st_blocks  see also: https://github.com/haskell/unix/pull/78/files
  -- these assumes Linux >= 2.6
  , accessTimeHiRes :: TimeSpec -- st_atim
  , modificationTimeHiRes :: TimeSpec -- st_mtim
  , statusChangeTimeHiRes :: TimeSpec -- st_ctim
  }
  deriving (Eq, Show)

instance Storable FileStat where
  sizeOf _ = #size struct stat

  alignment _ = #alignment struct stat

  peek ptr = do
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

-- | Reads a file status of a given file.
--
-- Calls @lstat@.
getFileStat :: FilePath -> IO FileStat
getFileStat path =
  alloca $ \buf ->
    withFilePath path $ \cpath -> do
      throwErrnoPathIfMinus1Retry_ "getFileStat" path (lstat cpath (castPtr buf))
      peek buf
