module Main where

-- TODO reexport things from System.Fuse3 to reduce imports
import CLoff
import Control.Exception (SomeException, bracket, tryJust)
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign (Ptr, allocaBytes, with)
import Foreign.C (CInt(CInt), CSize(CSize), CUInt(CUInt), Errno(Errno), eIO, eOK, eOPNOTSUPP)
import GHC.IO.Exception (IOException(IOError, ioe_errno))
import System.Fuse3
import System.IO (SeekMode, hPrint, stderr)
import System.Linux.XAttr (lCreateXAttr, lGetXAttr, lListXAttr, lRemoveXAttr, lReplaceXAttr, lSetXAttr)
import System.Posix.Directory (closeDirStream, createDirectory, openDirStream, readDirStream, removeDirectory)
import System.Posix.Files (createDevice, createLink, createNamedPipe, createSymbolicLink, readSymbolicLink, removeLink, rename, setFdSize, setFileCreationMask, setFileMode, setFileSize, setSymbolicLinkOwnerAndGroup, setSymbolicLinkTimesHiRes)
import System.Posix.IO (OpenFileFlags, OpenMode(WriteOnly), closeFd, defaultFileFlags, exclusive, fdSeek, openFd)
import System.Posix.Types (ByteCount, COff(COff), CSsize(CSsize), DeviceID, Fd(Fd), FileMode, FileOffset, GroupID, UserID)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

-- | Attempts to extract an `Errno` from an `IOError` assuming it is
-- constructed with `errnoToIOError` (typically via `throwErrno`).
--
-- TODO move this to Internal.hs
ioErrorToErrno :: IOError -> Maybe Errno
ioErrorToErrno IOError{ioe_errno=Just e} = Just $ Errno e
ioErrorToErrno _ = Nothing

tryErrno :: IO a -> IO (Either Errno a)
tryErrno = tryJust ioErrorToErrno

tryErrno_ :: IO a -> IO Errno
tryErrno_ = fmap (either id (const eOK)) . tryErrno

foreign import ccall "pread"
  c_pread :: CInt -> Ptr a -> CSize -> COff -> IO CSsize

foreign import ccall "pwrite"
  c_pwrite :: CInt -> Ptr a -> CSize -> COff -> IO CSsize

foreign import ccall "posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt

foreign import ccall "copy_file_range"
  c_copy_file_range :: CInt -> Ptr CLoff -> CInt -> Ptr CLoff -> CSize -> CUInt -> IO CSsize

xmpInit :: FuseConfig -> IO FuseConfig
xmpInit cfg = pure $ cfg
  { useIno = True
  -- Pick up changes from lower filesystem right away. This is
  -- also necessary for better hardlink support. When the kernel
  -- calls the unlink() handler, it does not know the inode of
  -- the to-be-removed entry and can therefore not invalidate
  -- the cache of the associated inode - resulting in an
  -- incorrect st_nlink value being reported for any remaining
  -- hardlinks to this inode.
  , entryTimeout = 0
  , attrTimeout = 0
  , negativeTimeout = 0
  }

xmpGetattr :: FilePath -> IO (Either Errno FileStat)
xmpGetattr = tryErrno . getFileStat

xmpAccess :: FilePath -> AccessMode -> IO Errno
xmpAccess = access

xmpReadlink :: FilePath -> IO (Either Errno FilePath)
xmpReadlink = tryErrno . readSymbolicLink

xmpReaddir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
xmpReaddir path = tryErrno
  $ bracket (openDirStream path) closeDirStream
  $ \dp -> fmap reverse $ flip fix []
  $ \loop acc -> do
    entry <- readDirStream dp
    if null entry
      then pure acc
      else do
        st <- getFileStat $ if null path then entry else path <> "/" <> entry
        loop $ (entry, st) : acc

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
xmpRead (Fd fd) size offset = tryErrno $ do
  -- TODO compare performance vs mallocBytes + unsafePackMallocCString
  allocaBytes (fromIntegral size) $ \buf -> do
    readBytes <- c_pread fd buf size offset
    B.packCStringLen (buf, fromIntegral readBytes)

xmpWrite :: Fd -> ByteString -> FileOffset -> IO (Either Errno CInt)
xmpWrite (Fd fd) bs offset = tryErrno $
  BU.unsafeUseAsCStringLen bs $ \(buf, size) ->
    -- truncate CSsize (ssize_t = 64bit) to CInt (32bit) because there is no other way
    fromIntegral <$> c_pwrite fd buf (fromIntegral size) offset

xmpStatfs :: FilePath -> IO (Either Errno FileSystemStats)
xmpStatfs = tryErrno . getFileSystemStats

xmpRelease :: Fd -> IO ()
xmpRelease = closeFd

xmpFsync :: IO Errno
-- Just a stub. This method is optional and can safely be left unimplemented
xmpFsync = pure eOK

xmpFallocate :: Fd -> FileMode -> FileOffset -> FileOffset -> IO Errno
xmpFallocate _ mode _ _ | mode /= 0 = pure eOPNOTSUPP
xmpFallocate (Fd fd) _ offset len =
  -- currently `System.Posix.Fcntl.fileAllocate` is broken
  -- (see https://github.com/haskell/unix/issues/156)
  -- so a custom implementation is used here
  --
  -- posix_fallocate(3) returns error number instead of setting errno
  Errno <$> c_posix_fallocate fd offset len

xmpSetxattr :: FilePath -> String -> ByteString -> SetxattrFlag -> IO Errno
xmpSetxattr path name value flag = tryErrno_ $ go path name value
  where
  go = case flag of
    SetxattrDefault -> lSetXAttr
    SetxattrCreate  -> lCreateXAttr
    SetxattrReplace -> lReplaceXAttr

xmpGetxattr :: FilePath -> String -> IO (Either Errno ByteString)
xmpGetxattr path name = tryErrno $ lGetXAttr path name

xmpListxattr :: FilePath -> IO (Either Errno [String])
xmpListxattr path = tryErrno $ lListXAttr path

xmpRemovexattr :: FilePath -> String -> IO Errno
xmpRemovexattr path name = tryErrno_ $ lRemoveXAttr path name

xmpCopyFileRange :: Fd -> FileOffset -> Fd -> FileOffset -> ByteCount -> IO (Either Errno CSsize)
xmpCopyFileRange (Fd fdIn) offIn (Fd fdOut) offOut len =
  tryErrno $
  with (fromIntegral offIn) $ \plOffIn ->
  with (fromIntegral offOut) $ \plOffOut -> do
    c_copy_file_range fdIn plOffIn fdOut plOffOut len 0
    -- the example of libfuse closes the fds but I don't think it is correct

xmpLseek :: Fd -> SeekMode -> FileOffset -> IO (Either Errno FileOffset)
xmpLseek fd mode offset = tryErrno $ fdSeek fd mode offset

xmpOper :: FuseOperations Fd
xmpOper = defaultFuseOps
  { fuseInit          = Just xmpInit
  , fuseGetattr       = Just $ \path _ -> xmpGetattr path
  , fuseAccess        = Just xmpAccess
  , fuseReadlink      = Just xmpReadlink
  , fuseReaddir       = Just $ \path _ -> xmpReaddir path
  , fuseMknod         = Just $ \path _ mode rdev -> xmpMknod path mode rdev
  , fuseMkdir         = Just xmpMkdir
  , fuseSymlink       = Just xmpSymlink
  , fuseUnlink        = Just xmpUnlink
  , fuseRmdir         = Just xmpRmdir
  , fuseRename        = Just xmpRename
  , fuseLink          = Just xmpLink
  , fuseChmod         = Just $ \path _ mode -> xmpChmod path mode
  , fuseChown         = Just $ \path _ uid gid -> xmpChown path uid gid
  , fuseTruncate      = Just xmpTruncate
  -- , fuseUtimens       = Just xmpUtimens
  , fuseOpen          = Just xmpOpen
  -- , fuseCreate        = Just xmpCreate
  , fuseRead          = Just $ \_ -> xmpRead
  , fuseWrite         = Just $ \_ -> xmpWrite
  , fuseStatfs        = Just xmpStatfs
  , fuseRelease       = Just $ \_ -> xmpRelease
  , fuseFsync         = Just $ \_ _ _ -> xmpFsync
  -- , fuseFallocate     = Just xmpFallocate
  -- , fuseSetxattr      = Just xmpSetxattr
  -- , fuseGetxattr      = Just xmpGetxattr
  -- , fuseListxattr     = Just xmpListxattr
  -- , fuseRemovexattr   = Just xmpRemovexattr
  -- , fuseCopyFileRange = Just xmpCopyFileRange
  -- , fuseLseek         = Just xmpLseek
  }

main :: IO ()
main = do
  _ <- setFileCreationMask 0
  fuseMain xmpOper (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
