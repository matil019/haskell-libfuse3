{-# LANGUAGE RecordWildCards #-}
-- | @struct statvfs@ in Haskell.
module System.LibFuse3.FileSystemStats where

#include <sys/statvfs.h>

import Foreign (Ptr, Storable, alloca, peek, peekByteOff, pokeByteOff)
import Foreign.C (CInt(CInt), CString, CULong, throwErrnoIfMinus1Retry_)
import System.Posix.Error (throwErrnoPathIfMinus1Retry_)
import System.Posix.Internals (withFilePath)
import System.Posix.Types (CFsBlkCnt, CFsFilCnt, Fd(Fd))

import qualified Foreign

-- | Passed to 'fuseGetFileSystemStats'.
--
-- The 'Storable' instance targets C @struct statvfs@.
--
-- @f_favail@, @f_fsid@ and @f_flag@ fields are ignored.
data FileSystemStats = FileSystemStats
  { -- | Filesystem block size. @f_bsize@
    blockSize :: CULong
  , -- | Fragment size. @f_frsize@
    fragmentSize :: CULong
  , -- | Size of the filesystem in @f_frsize@ units. @f_blocks@
    blockCount :: CFsBlkCnt
  , -- | Number of free blocks. @f_bfree@
    blocksFree :: CFsBlkCnt
  , -- | Number of free blocks for unprivileged users. @f_bavail@
    blocksAvailable :: CFsBlkCnt
  , -- | Number of inodes (file nodes). @f_files@
    fileCount :: CFsFilCnt
  , -- | Number of free inodes. @f_ffree@
    filesFree :: CFsFilCnt
  , -- | Maximum filename length. @f_namemax@
    maxNameLength :: CULong
  }
  deriving (Eq, Show)

instance Storable FileSystemStats where
  sizeOf _ = #size struct statvfs

  alignment _ = #alignment struct statvfs

  peek ptr = do
    blockSize       <- (#peek struct statvfs, f_bsize) ptr
    fragmentSize    <- (#peek struct statvfs, f_frsize) ptr
    blockCount      <- (#peek struct statvfs, f_blocks) ptr
    blocksFree      <- (#peek struct statvfs, f_bfree) ptr
    blocksAvailable <- (#peek struct statvfs, f_bavail) ptr
    fileCount       <- (#peek struct statvfs, f_files) ptr
    filesFree       <- (#peek struct statvfs, f_ffree) ptr
    maxNameLength   <- (#peek struct statvfs, f_namemax) ptr
    pure FileSystemStats{..}

  poke ptr FileSystemStats{..} = do
    (#poke struct statvfs, f_bsize)   ptr blockSize
    (#poke struct statvfs, f_frsize)  ptr fragmentSize
    (#poke struct statvfs, f_blocks)  ptr blockCount
    (#poke struct statvfs, f_bfree)   ptr blocksFree
    (#poke struct statvfs, f_bavail)  ptr blocksAvailable
    (#poke struct statvfs, f_files)   ptr fileCount
    (#poke struct statvfs, f_ffree)   ptr filesFree
    (#poke struct statvfs, f_namemax) ptr maxNameLength

-- | Gets filesystem statistics.
--
-- Calls @statvfs@.
getFileSystemStats
  :: FilePath  -- ^ A path of any file within the filesystem
  -> IO FileSystemStats
getFileSystemStats path =
  alloca $ \buf ->
  withFilePath path $ \cpath -> do
    throwErrnoPathIfMinus1Retry_ "getFileSystemStats" path (c_statvfs cpath buf)
    peek buf

-- | Gets filesystem statistics.
--
-- Calls @fstatvfs@.
getFileSystemStatsFd :: Fd -> IO FileSystemStats
getFileSystemStatsFd fd =
  alloca $ \buf -> do
    throwErrnoIfMinus1Retry_ "getFileSystemStatsFd" (c_fstatvfs fd buf)
    peek buf

foreign import ccall safe "fstatvfs"
  c_fstatvfs :: Fd -> Ptr FileSystemStats -> IO CInt

foreign import ccall safe "statvfs"
  c_statvfs :: CString -> Ptr FileSystemStats -> IO CInt
