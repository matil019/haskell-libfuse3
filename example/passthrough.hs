-- |
-- Copyright : (The original C)   2001-2007  Miklos Szeredi <miklos@szeredi.hu>
--                                2011       Sebastian Pipping <sebastian@pipping.org>
--             (The Haskell port) 2020 yohashi
-- License   : GPL-2
--
-- This file system mirrors the existing file system hierarchy of the
-- system, starting at the root file system. This is implemented by
-- just "passing through" all requests to the corresponding user-space
-- libc functions. Its performance is terrible.
--
-- This is a port of the C program distributed with the official libfuse.
-- See \"example/passthrough.c\" in the distribution.
module Main where

import CLoff
import Control.Exception (SomeException, bracket)
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Void (Void)
import Foreign (Ptr, with)
import Foreign.C (CInt(CInt), CSize(CSize), CUInt(CUInt), Errno(Errno), eIO, eOK, eOPNOTSUPP)
import System.IO (SeekMode, hPrint, stderr)
import System.LibFuse3
import System.Posix.Directory (closeDirStream, createDirectory, openDirStream, readDirStream, removeDirectory)
import System.Posix.Files (createDevice, createLink, createNamedPipe, createSymbolicLink, readSymbolicLink, removeLink, rename, setFdSize, setFileCreationMask, setFileMode, setFileSize, setSymbolicLinkOwnerAndGroup, setSymbolicLinkTimesHiRes)
import System.Posix.IO (OpenFileFlags, OpenMode(WriteOnly), closeFd, defaultFileFlags, exclusive, fdSeek, openFd)
import System.Posix.Types (ByteCount, COff(COff), CSsize(CSsize), DeviceID, Fd(Fd), FileMode, FileOffset, GroupID, UserID)

import qualified System.LibFuse3.FuseConfig as FuseConfig
import qualified XAttr

foreign import ccall "posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt

foreign import ccall "copy_file_range"
  c_copy_file_range :: CInt -> Ptr CLoff -> CInt -> Ptr CLoff -> CSize -> CUInt -> IO CSsize

xmpInit :: FuseConfig -> IO FuseConfig
xmpInit cfg = pure $ cfg
  { FuseConfig.useIno = True
  -- Pick up changes from lower filesystem right away. This is
  -- also necessary for better hardlink support. When the kernel
  -- calls the unlink() handler, it does not know the inode of
  -- the to-be-removed entry and can therefore not invalidate
  -- the cache of the associated inode - resulting in an
  -- incorrect st_nlink value being reported for any remaining
  -- hardlinks to this inode.
  , FuseConfig.entryTimeout = 0
  , FuseConfig.attrTimeout = 0
  , FuseConfig.negativeTimeout = 0
  }

xmpGetattr :: FilePath -> IO (Either Errno FileStat)
xmpGetattr = tryErrno . getFileStat

xmpAccess :: FilePath -> AccessMode -> IO Errno
xmpAccess = accessErrno

xmpReadlink :: FilePath -> IO (Either Errno FilePath)
xmpReadlink = tryErrno . readSymbolicLink

xmpReaddir :: FilePath -> IO (Either Errno [(String, Maybe FileStat)])
xmpReaddir path = tryErrno
  $ bracket (openDirStream path) closeDirStream
  $ \dp -> fmap reverse $ flip fix []
  $ \loop acc -> do
    entry <- readDirStream dp
    if null entry
      then pure acc
      else loop $ (entry, Nothing) : acc

xmpMknod :: FilePath -> FileMode -> DeviceID -> IO Errno
xmpMknod path mode rdev = tryErrno_ $ case fileModeToEntryType mode of
  RegularFile -> bracket (openFd path WriteOnly (Just mode) (defaultFileFlags{exclusive=True})) closeFd (\_ -> pure ())
  Directory -> createDirectory path mode
  NamedPipe -> createNamedPipe path mode
  _ -> createDevice path mode rdev

xmpMkdir :: FilePath -> FileMode -> IO Errno
xmpMkdir path mode = tryErrno_ $ createDirectory path mode

xmpUnlink :: FilePath -> IO Errno
xmpUnlink = tryErrno_ . removeLink

xmpRmdir :: FilePath -> IO Errno
xmpRmdir = tryErrno_ . removeDirectory

xmpSymlink :: FilePath -> FilePath -> IO Errno
xmpSymlink from to = tryErrno_ $ createSymbolicLink from to

xmpRename :: FilePath -> FilePath -> IO Errno
xmpRename from to = tryErrno_ $ rename from to

xmpLink :: FilePath -> FilePath -> IO Errno
xmpLink from to = tryErrno_ $ createLink from to

xmpChmod :: FilePath -> FileMode -> IO Errno
xmpChmod path mode = tryErrno_ $ setFileMode path mode

xmpChown :: FilePath -> UserID -> GroupID -> IO Errno
xmpChown path uid gid = tryErrno_ $ setSymbolicLinkOwnerAndGroup path uid gid

xmpTruncate :: FilePath -> Maybe Fd -> FileOffset -> IO Errno
xmpTruncate path mfd size = tryErrno_ $ case mfd of
  Just fd -> setFdSize fd size
  Nothing -> setFileSize path size

xmpUtimens :: FilePath -> POSIXTime -> POSIXTime -> IO Errno
xmpUtimens path atime mtime = tryErrno_ $ setSymbolicLinkTimesHiRes path atime mtime

xmpCreate :: FilePath -> OpenMode -> FileMode -> OpenFileFlags -> IO (Either Errno Fd)
xmpCreate path omode fmode flags = tryErrno $ openFd path omode (Just fmode) flags

xmpOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Fd)
xmpOpen path mode flags = tryErrno $ openFd path mode Nothing flags

xmpRead :: Fd -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
xmpRead fd size offset = tryErrno $ pread fd size offset

xmpWrite :: Fd -> ByteString -> FileOffset -> IO (Either Errno CInt)
xmpWrite fd bs offset = tryErrno $ fromIntegral <$> pwrite fd bs offset

xmpStatfs :: FilePath -> IO (Either Errno FileSystemStats)
xmpStatfs = tryErrno . getFileSystemStats

xmpRelease :: Fd -> IO ()
xmpRelease = closeFd

xmpFsync :: IO Errno
-- Just a stub. This method is optional and can safely be left unimplemented
xmpFsync = pure eOK

xmpFallocate :: Fd -> CInt -> FileOffset -> FileOffset -> IO Errno
xmpFallocate _ mode _ _ | mode /= 0 = pure eOPNOTSUPP
xmpFallocate (Fd fd) _ offset len =
  -- currently `System.Posix.Fcntl.fileAllocate` is broken
  -- (see https://github.com/haskell/unix/issues/156)
  -- so a custom implementation is used here
  --
  -- posix_fallocate(3) returns error number instead of setting errno
  Errno <$> c_posix_fallocate fd offset len

xmpSetxattr :: FilePath -> String -> ByteString -> SetxattrFlag -> IO Errno
xmpSetxattr path name value flag = tryErrno_ $ XAttr.set path name value flag

xmpGetxattr :: FilePath -> String -> IO (Either Errno ByteString)
xmpGetxattr path name = tryErrno $ XAttr.get path name

xmpListxattr :: FilePath -> IO (Either Errno [String])
xmpListxattr path = tryErrno $ XAttr.list path

xmpRemovexattr :: FilePath -> String -> IO Errno
xmpRemovexattr path name = tryErrno_ $ XAttr.remove path name

xmpCopyFileRange :: Fd -> FileOffset -> Fd -> FileOffset -> ByteCount -> CUInt -> IO (Either Errno CSsize)
xmpCopyFileRange (Fd fdIn) offIn (Fd fdOut) offOut len flags =
  tryErrno $
  with (fromIntegral offIn) $ \plOffIn ->
  with (fromIntegral offOut) $ \plOffOut -> do
    c_copy_file_range fdIn plOffIn fdOut plOffOut len flags
    -- the example of libfuse closes the fds but I don't think it is correct

xmpLseek :: Fd -> SeekMode -> FileOffset -> IO (Either Errno FileOffset)
xmpLseek fd mode offset = tryErrno $ fdSeek fd mode offset

xmpOper :: FuseOperations Fd Void
xmpOper = defaultFuseOperations
  { fuseInit          = Just xmpInit
  , fuseGetattr       = Just $ \path _ -> xmpGetattr path
  , fuseAccess        = Just xmpAccess
  , fuseReadlink      = Just xmpReadlink
  , fuseReaddir       = Just $ \path _ -> xmpReaddir path
  , fuseMknod         = Just xmpMknod
  , fuseMkdir         = Just xmpMkdir
  , fuseSymlink       = Just xmpSymlink
  , fuseUnlink        = Just xmpUnlink
  , fuseRmdir         = Just xmpRmdir
  , fuseRename        = Just xmpRename
  , fuseLink          = Just xmpLink
  , fuseChmod         = Just $ \path _ mode -> xmpChmod path mode
  , fuseChown         = Just $ \path _ uid gid -> xmpChown path uid gid
  , fuseTruncate      = Just xmpTruncate
  , fuseUtimens       = Just $ \path _ atime mtime -> xmpUtimens path (timeSpecToPOSIXTime atime) (timeSpecToPOSIXTime mtime)
  , fuseOpen          = Just xmpOpen
  , fuseCreate        = Just xmpCreate
  , fuseRead          = Just $ \_ -> xmpRead
  , fuseWrite         = Just $ \_ -> xmpWrite
  , fuseStatfs        = Just xmpStatfs
  , fuseRelease       = Just $ \_ -> xmpRelease
  , fuseFsync         = Just $ \_ _ _ -> xmpFsync
  , fuseFallocate     = Just $ \_ fd mode offset len -> xmpFallocate fd mode offset len
  , fuseSetxattr      = Just xmpSetxattr
  , fuseGetxattr      = Just xmpGetxattr
  , fuseListxattr     = Just xmpListxattr
  , fuseRemovexattr   = Just xmpRemovexattr
  , fuseCopyFileRange = Just $ \_ fdIn offIn _ fdOut offOut size flags -> xmpCopyFileRange fdIn offIn fdOut offOut size (fromIntegral flags)
  , fuseLseek         = Just $ \_ fd off mode -> xmpLseek fd mode off
  }

main :: IO ()
main = do
  _ <- setFileCreationMask 0
  fuseMain xmpOper (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
