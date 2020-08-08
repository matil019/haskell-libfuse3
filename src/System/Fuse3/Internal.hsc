{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK no-print-missing-docs #-}
{-| Based on `System.Fuse` in the package "HFuse-0.2.5.0".

TODO move to LICENSE

Below is the @LICENSE@ of "HFuse":

Copyright (c) Jérémy Bobbio
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the University nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
-}
module System.Fuse3.Internal where

import Control.Exception (Exception, bracket, bracket_, finally, handle)
import Control.Monad ((>=>), unless, void)
import Data.Bits ((.&.), (.|.), Bits)
import Data.Foldable (traverse_)
import Foreign
  ( FunPtr
  , Ptr
  , allocaBytes
  , callocBytes
  , castPtrToStablePtr
  , castStablePtrToPtr
  , copyArray
  , deRefStablePtr
  , free
  , freeHaskellFunPtr
  , freeStablePtr
  , maybePeek
  , newStablePtr
  , nullFunPtr
  , nullPtr
  , peek
  , peekByteOff
  , poke
  , pokeByteOff
  , pokeElemOff
  , with
  , withArray
  , withMany
  )
import Foreign.C (CDouble(CDouble), CInt(CInt), CStringLen, Errno(Errno), eOK, getErrno, peekCString, withCString, withCStringLen)
import GHC.IO.Handle (hDuplicateTo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess), exitFailure, exitSuccess)
import System.Fuse3.FileStat (FileStat)
import System.Fuse3.FileSystemStats (FileSystemStats)
import System.IO (IOMode(ReadMode, WriteMode), stderr, stdin, stdout, withFile)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files (blockSpecialMode, characterSpecialMode, directoryMode, namedPipeMode, regularFileMode, socketMode, symbolicLinkMode)
import System.Posix.IO (OpenFileFlags(OpenFileFlags), OpenMode(ReadOnly, ReadWrite, WriteOnly))
import System.Posix.Internals (c_access)
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.Types (ByteCount, COff(COff), DeviceID, FileMode, FileOffset, GroupID, UserID)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.Fuse3.Internal.C as C
import qualified System.Posix.IO
import qualified System.Posix.Signals as Signals

#include <fuse.h>
#include <fuse_lowlevel.h>

-- | The Unix type of a node in the filesystem.
data EntryType
  = Unknown            -- ^ Unknown entry type
  | NamedPipe
  | CharacterSpecial
  | Directory
  | BlockSpecial
  | RegularFile
  | SymbolicLink
  | Socket
  deriving (Eq, Show)

-- | Converts an 'EntryType' into the corresponding POSIX 'FileMode'.
entryTypeToFileMode :: EntryType -> FileMode
entryTypeToFileMode Unknown          = 0
entryTypeToFileMode NamedPipe        = namedPipeMode
entryTypeToFileMode CharacterSpecial = characterSpecialMode
entryTypeToFileMode Directory        = directoryMode
entryTypeToFileMode BlockSpecial     = blockSpecialMode
entryTypeToFileMode RegularFile      = regularFileMode
entryTypeToFileMode SymbolicLink     = symbolicLinkMode
entryTypeToFileMode Socket           = socketMode

-- | Decodes `EntryType` from a `FileMode`.
fileModeToEntryType :: FileMode -> EntryType
fileModeToEntryType mode
  | fileType == namedPipeMode        = NamedPipe
  | fileType == characterSpecialMode = CharacterSpecial
  | fileType == directoryMode        = Directory
  | fileType == blockSpecialMode     = BlockSpecial
  | fileType == regularFileMode      = RegularFile
  | fileType == symbolicLinkMode     = SymbolicLink
  | fileType == socketMode           = Socket
  | otherwise = Unknown
  where
  fileType = mode .&. (#const S_IFMT)

-- | Passed to `fuseFsync` and `fuseFsyncdir`.
data SyncType
  -- | Synchronize both file content and metadata.
  = FullSync
  -- | Synchronize only the file content.
  | DataSync
  deriving (Eq, Show)

-- | The query type of @access@. Passed to `fuseAccess`.
data AccessMode
  -- | File existence (@F_OK@)
  = FileOK
  -- | Reading, writing and executing permissions (@R_OK@, @W_OK@ and @X_OK@, resp.)
  | PermOK Bool Bool Bool
  deriving (Eq, Show)

-- | Tests if access permissions to the file is granted or the file exists.
--
-- Calls @access@. Compared to `System.Posix.Files.fileAccess` and
-- `System.Posix.Files.fileExist`, this function doesn't translate the errno and just
-- returns it.
--
-- @pure `eOK`@ on success and `getErrno` on failure. Never throws an exception.
access :: FilePath -> AccessMode -> IO Errno
access path mode = withCString path $ \cPath -> do
  let cMode = case mode of
        FileOK -> #const F_OK
        PermOK r w x ->
          (if r then (#const R_OK) else 0) .|.
          (if w then (#const W_OK) else 0) .|.
          (if x then (#const X_OK) else 0)
  ret <- c_access cPath cMode
  if ret == 0
    then pure eOK
    else getErrno

-- TODO move to another module?
-- | @testBitSet bits mask@ is @True@ iff all bits in @mask@ are set in @bits@.
--
-- @
-- testBitSet bits mask ≡ bits .&. mask == mask
-- @
testBitSet :: Bits a => a -> a -> Bool
testBitSet bits mask = bits .&. mask == mask

-- memo: when adding a new field, make sure to update withCFuseOperations
-- | The file system operations.
--
-- Each field is named against @struct fuse_operations@ in @fuse.h@.
--
-- @fh@ is the file handle type returned by `fuseOpen` and `fuseOpendir`, and
-- subsequently passed to all other file operations.
--
-- TODO distinguish file handle and directory handle? (with Either?)
data FuseOperations fh = FuseOperations
  { -- | Implements 'System.Posix.Files.getSymbolicLinkStatus' operation (POSIX @lstat(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open.
    fuseGetattr :: Maybe (FilePath -> Maybe fh -> IO (Either Errno FileStat))

  , -- | Implements 'System.Posix.Files.readSymbolicLink' operation (POSIX @readlink(2)@).
    --
    -- The returned 'FilePath' might be truncated depending on caller buffer size.
    fuseReadlink :: Maybe (FilePath -> IO (Either Errno FilePath))

  , -- | Implements 'System.Posix.Files.createDevice' (POSIX @mknod(2)@).
    --
    -- This function will also be called for regular file creation if `fuseCreate` is not defined.
    fuseMknod :: Maybe (FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno)

  , -- | Implements 'System.Posix.Directory.createDirectory' (POSIX @mkdir(2)@).
    fuseMkdir :: Maybe (FilePath -> FileMode -> IO Errno)

  , -- | Implements 'System.Posix.Files.removeLink' (POSIX @unlink(2)@).
    fuseUnlink :: Maybe (FilePath -> IO Errno)

  , -- | Implements 'Ststen.Posix.Directory.removeDirectory' (POSIX @rmdir(2)@).
    fuseRmdir :: Maybe (FilePath -> IO Errno)

  , -- | Implements 'System.Posix.Files.createSymbolicLink' (POSIX @symlink(2)@).
    fuseSymlink :: Maybe (FilePath -> FilePath -> IO Errno)

  , -- | Implements 'System.Posix.Files.rename' (POSIX @rename(2)@).
    fuseRename :: Maybe (FilePath -> FilePath -> IO Errno)

  , -- | Implements 'System.Posix.Files.createLink' (POSIX @link(2)@).
    fuseLink :: Maybe (FilePath -> FilePath -> IO Errno)

  , -- | Implements 'System.Posix.Files.setFileMode' (POSIX @chmod(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open.
    fuseChmod :: Maybe (FilePath -> Maybe fh -> FileMode -> IO Errno)

  , -- | Implements 'System.Posix.Files.setOwnerAndGroup' (POSIX @chown(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open. This method is expected to reset the setuid and setgid
    -- bits.
    --
    -- TODO FUSE_CAP_HANDLE_KILLPRIV?
    fuseChown :: Maybe (FilePath -> Maybe fh -> UserID -> GroupID -> IO Errno)

  , -- | Implements 'System.Posix.Files.setFileSize' (POSIX @truncate(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open. This method is expected to reset the setuid and setgid
    -- bits.
    --
    -- TODO FUSE_CAP_HANDLE_KILLPRIV?
    fuseTruncate :: Maybe (FilePath -> Maybe fh -> FileOffset -> IO Errno)

  , -- | Implements 'System.Posix.Files.openFd' (POSIX @open(2)@).  On success, returns
    -- 'Right' of a filehandle-like value that will be passed to future file operations; on
    -- failure, returns 'Left' of the appropriate 'Errno'.
    --
    --   * Creation flags will be filtered out / handled by the kernel.
    --   * Access modes should be used by this to check if the operation is permitted.
    --   * TODO if "writeback caching" is relevant, describe their caveats
    --     http://libfuse.github.io/doxygen/structfuse__operations.html#a14b98c3f7ab97cc2ef8f9b1d9dc0709d
    --
    -- TODO expose FuseFileInfo to allow setting direct_io and keep_cache?
    -- TODO what about fuse_conn_info.capable stuff?
    fuseOpen :: Maybe (FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh))

  , -- | Implements Unix98 @pread(2)@.
    --
    -- It differs from 'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
    fuseRead :: Maybe (FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString))

  , -- | Implements Unix98 @pwrite(2)@.
    --
    -- It differs from 'System.Posix.Files.fdWrite' by the explicit 'FileOffset' argument.
    --
    -- This method is expected to reset the setuid and setgid bits.
    --
    -- TODO FUSE_CAP_HANDLE_KILLPRIV?
    fuseWrite :: Maybe (FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno CInt))

  , -- | Implements @statfs(2)@. TODO describe ignored fields
    fuseStatfs :: Maybe (String -> IO (Either Errno FileSystemStats))

  , -- | Called when @close(2)@ has been called on an open file.
    --
    -- Note: this does not mean that the file is released.  This function may be called more
    -- than once for each @open(2)@.  The return value is passed on to the @close(2)@ system
    -- call.
    fuseFlush :: Maybe (FilePath -> fh -> IO Errno)

  , -- | Called when an open file has all file descriptors closed and all memory mappings
    -- unmapped.
    --
    -- For every @open@ call there will be exactly one @release@ call with the same flags.
    -- It is possible to have a file opened more than once, in which case only the last
    -- release will mean that no more reads or writes will happen on the file.
    fuseRelease :: Maybe (FilePath -> fh -> IO ())

  , -- | Implements @fsync(2)@.
    fuseFsync :: Maybe (FilePath -> fh -> SyncType -> IO Errno)

    -- TODO , setxattr :: _
    -- TODO , getxattr :: _
    -- TODO , listxattr :: _
    -- TODO , removexattr :: _

  , -- | Implements @opendir(3)@.
    --
    -- This method should check if the open operation is permitted for this directory.
    fuseOpendir :: Maybe (FilePath -> IO (Either Errno fh))

  , -- | Implements @readdir(3)@.
    --
    -- The entire contents of the directory should be returned as a list of tuples
    -- (corresponding to the first mode of operation documented in @fuse.h@).
    fuseReaddir :: Maybe (FilePath -> fh -> IO (Either Errno [(FilePath, FileStat)]))

  , -- | Implements @closedir(3)@.
    fuseReleasedir :: Maybe (FilePath -> fh -> IO Errno)

  , -- | Synchronize the directory's contents; analogous to `fuseFsync`.
    fuseFsyncdir :: Maybe (FilePath -> fh -> SyncType -> IO Errno)

  , -- | Initializes the filesystem.  This is called before all other operations.
    --
    -- The filesystem may modify `FuseConfig` to configure the API.
    fuseInit :: Maybe (FuseConfig -> IO FuseConfig)

  , -- | Called on filesystem exit to allow cleanup.
    fuseDestroy :: Maybe (IO ())

  , -- | Implements 'System.Posix.Files.fileAccess' and 'System.Posix.Files.fileExist
    -- (POSIX @access(2)@).
    --
    -- Checks file access permissions as requested by an `AccessMode`.
    --
    -- If the @default_permissions@ mount option is given, this method is not called. This
    -- method is also not called under Linux kernel versions 2.4.x
    --
    -- TODO add notes about @default_permissions@ to other relevant handlers
    fuseAccess :: Maybe (FilePath -> AccessMode -> IO Errno)

    -- TODO , create :: _
    -- TODO , lock :: _
    -- TODO , utimens :: _
    -- TODO , bmap :: _
    -- TODO , ioctl :: _
    -- TODO , poll :: _
    -- TODO , write_buf :: _
    -- TODO , read_buf :: _
    -- TODO , flock :: _
    -- TODO , fallocate :: _
    -- TODO , copy_file_range :: _
    -- TODO , lseek :: _
  }

-- | An empty set of operations whose fields are @Nothing@.
defaultFuseOps :: FuseOperations fh
defaultFuseOps = FuseOperations
  { fuseGetattr = Nothing
  , fuseReadlink = Nothing
  , fuseMknod = Nothing
  , fuseMkdir = Nothing
  , fuseUnlink = Nothing
  , fuseRmdir = Nothing
  , fuseSymlink = Nothing
  , fuseRename = Nothing
  , fuseLink = Nothing
  , fuseChmod = Nothing
  , fuseChown = Nothing
  , fuseTruncate = Nothing
  , fuseOpen = Nothing
  , fuseRead = Nothing
  , fuseWrite = Nothing
  , fuseStatfs = Nothing
  , fuseFlush = Nothing
  , fuseRelease = Nothing
  , fuseFsync = Nothing
  , fuseOpendir = Nothing
  , fuseReaddir = Nothing
  , fuseReleasedir = Nothing
  , fuseFsyncdir = Nothing
  , fuseInit = Nothing
  , fuseDestroy = Nothing
  , fuseAccess = Nothing
  }

data FuseConfig = FuseConfig
  { -- | @entry_timeout@
    entryTimeout :: Double
  , -- | @negative_timeout@
    negativeTimeout :: Double
  , -- | @attr_timeout@
    attrTimeout :: Double
  , -- | @use_ino@
    useIno :: Bool
  }
  deriving (Eq, Show)

toCFuseConfig :: FuseConfig -> C.FuseConfig
toCFuseConfig FuseConfig{..} = C.FuseConfig
  { C.entryTimeout = CDouble entryTimeout
  , C.negativeTimeout = CDouble negativeTimeout
  , C.attrTimeout = CDouble attrTimeout
  , C.useIno = if useIno then 1 else 0
  }

fromCFuseConfig :: C.FuseConfig -> FuseConfig
fromCFuseConfig C.FuseConfig{..} = FuseConfig
  { entryTimeout = unCDouble entryTimeout
  , negativeTimeout = unCDouble negativeTimeout
  , attrTimeout = unCDouble attrTimeout
  , useIno = useIno /= 0
  }
  where
  unCDouble (CDouble x) = x

-- | Allocates a @fuse_args@ struct to hold commandline arguments.
withFuseArgs :: String -> [String] -> (Ptr C.FuseArgs -> IO b) -> IO b
withFuseArgs prog args f =
  let allArgs = (prog:args)
      argc = length allArgs
  in
  withMany withCString allArgs $ \cArgs ->
  withArray cArgs $ \pArgv ->
  -- TODO call FUSE_ARGS_INIT instead?
  allocaBytes (#size struct fuse_args) $ \fuseArgs -> do
    (#poke struct fuse_args, argc) fuseArgs argc
    (#poke struct fuse_args, argv) fuseArgs pArgv
    (#poke struct fuse_args, allocated) fuseArgs (0::CInt)
    f fuseArgs `finally` C.fuse_opt_free_args fuseArgs

-- | Allocates a @fuse_operations@ struct and pokes `FuseOperations` into it.
--
-- Each field of `FuseOperations` is converted into a C function pointer and is assigned
-- to a corresponding field of @struct fuse_operations@.
withCFuseOperations
  :: forall fh e b
   . Exception e
  => FuseOperations fh              -- ^ A set of file system operations.
  -> (e -> IO Errno)                -- ^ An error handler that converts a Haskell exception to @errno@.
  -> (Ptr C.FuseOperations -> IO b) --
  -> IO b
withCFuseOperations ops handler cont =
  bracket (callocBytes (#size struct fuse_operations)) free $ \pOps ->
    withC C.mkGetattr    wrapGetattr    (fuseGetattr ops)    $ (#poke struct fuse_operations, getattr) pOps >=> \_ ->
    withC C.mkReadlink   wrapReadlink   (fuseReadlink ops)   $ (#poke struct fuse_operations, readlink) pOps >=> \_ ->
    withC C.mkMknod      wrapMknod      (fuseMknod ops)      $ (#poke struct fuse_operations, mknod) pOps >=> \_ ->
    withC C.mkMkdir      wrapMkdir      (fuseMkdir ops)      $ (#poke struct fuse_operations, mkdir) pOps >=> \_ ->
    withC C.mkUnlink     wrapUnlink     (fuseUnlink ops)     $ (#poke struct fuse_operations, unlink) pOps >=> \_ ->
    withC C.mkRmdir      wrapRmdir      (fuseRmdir ops)      $ (#poke struct fuse_operations, rmdir) pOps >=> \_ ->
    withC C.mkSymlink    wrapSymlink    (fuseSymlink ops)    $ (#poke struct fuse_operations, symlink) pOps >=> \_ ->
    withC C.mkRename     wrapRename     (fuseRename ops)     $ (#poke struct fuse_operations, rename) pOps >=> \_ ->
    withC C.mkLink       wrapLink       (fuseLink ops)       $ (#poke struct fuse_operations, link) pOps >=> \_ ->
    withC C.mkChmod      wrapChmod      (fuseChmod ops)      $ (#poke struct fuse_operations, chmod) pOps >=> \_ ->
    withC C.mkChown      wrapChown      (fuseChown ops)      $ (#poke struct fuse_operations, chown) pOps >=> \_ ->
    withC C.mkTruncate   wrapTruncate   (fuseTruncate ops)   $ (#poke struct fuse_operations, truncate) pOps >=> \_ ->
    withC C.mkOpen       wrapOpen       (fuseOpen ops)       $ (#poke struct fuse_operations, open) pOps >=> \_ ->
    withC C.mkRead       wrapRead       (fuseRead ops)       $ (#poke struct fuse_operations, read) pOps >=> \_ ->
    withC C.mkWrite      wrapWrite      (fuseWrite ops)      $ (#poke struct fuse_operations, write) pOps >=> \_ ->
    withC C.mkStatfs     wrapStatfs     (fuseStatfs ops)     $ (#poke struct fuse_operations, statfs) pOps >=> \_ ->
    withC C.mkFlush      wrapFlush      (fuseFlush ops)      $ (#poke struct fuse_operations, flush) pOps >=> \_ ->
    withC C.mkRelease    wrapRelease    (fuseRelease ops)    $ (#poke struct fuse_operations, release) pOps >=> \_ ->
    withC C.mkFsync      wrapFsync      (fuseFsync ops)      $ (#poke struct fuse_operations, fsync) pOps >=> \_ ->
    withC C.mkOpendir    wrapOpendir    (fuseOpendir ops)    $ (#poke struct fuse_operations, opendir) pOps >=> \_ ->
    withC C.mkReaddir    wrapReaddir    (fuseReaddir ops)    $ (#poke struct fuse_operations, readdir) pOps >=> \_ ->
    withC C.mkReleasedir wrapReleasedir (fuseReleasedir ops) $ (#poke struct fuse_operations, releasedir) pOps >=> \_ ->
    withC C.mkFsyncdir   wrapFsyncdir   (fuseFsyncdir ops)   $ (#poke struct fuse_operations, fsyncdir) pOps >=> \_ ->
    withC C.mkInit       wrapInit       (fuseInit ops)       $ (#poke struct fuse_operations, init) pOps >=> \_ ->
    withC C.mkDestroy    wrapDestroy    (fuseDestroy ops)    $ (#poke struct fuse_operations, destroy) pOps >=> \_ ->
    withC C.mkAccess     wrapAccess     (fuseAccess ops)     $ (#poke struct fuse_operations, access) pOps >=> \_ ->
    cont pOps
  where
  -- convert a Haskell function to C one with @wrapMeth@, get its @FunPtr@, and loan it to a continuation
  withC :: (cfunc -> IO (FunPtr cfunc)) -> (hsfunc -> cfunc) -> Maybe hsfunc -> (FunPtr cfunc -> IO c) -> IO c
  withC mkMeth wrapMeth = maybeWithFun (withHaskellFunPtr mkMeth . wrapMeth)

  -- return negated errno as specified by fuse.h. also handle any Haskell exceptions
  handleAsFuseError :: IO Errno -> IO CInt
  handleAsFuseError = handleAsFuseErrorResult . fmap Left -- assumes eOK == 0

  -- return a (successful) result as positive int and a negated errno as negative int
  handleAsFuseErrorResult :: IO (Either Errno CInt) -> IO CInt
  handleAsFuseErrorResult = fmap (either (negate . unErrno) id) . handle (fmap Left . handler)

  wrapGetattr :: (FilePath -> Maybe fh -> IO (Either Errno FileStat)) -> C.CGetattr
  wrapGetattr go pFilePath pStat pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    go filePath mfh >>= \case
      Left errno -> pure errno
      Right stat -> do
        poke pStat stat
        pure eOK

  wrapReadlink :: (FilePath -> IO (Either Errno FilePath)) -> C.CReadlink
  wrapReadlink go pFilePath pBuf bufSize = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath >>= \case
      Left errno -> pure errno
      Right target -> do
        -- This will truncate target if it's longer than the buffer can hold,
        -- as specified by fuse.h
        pokeCStringLen0 (pBuf, (fromIntegral bufSize)) target
        pure eOK

  wrapMknod :: (FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno) -> C.CMknod
  wrapMknod go pFilePath mode dev = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath (fileModeToEntryType mode) mode dev

  wrapMkdir :: (FilePath -> FileMode -> IO Errno) -> C.CMkdir
  wrapMkdir go pFilePath mode = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath mode

  wrapUnlink :: (FilePath -> IO Errno) -> C.CUnlink
  wrapUnlink go pFilePath = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath

  wrapRmdir :: (FilePath -> IO Errno) -> C.CRmdir
  wrapRmdir go pFilePath = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath

  wrapSymlink :: (FilePath -> FilePath -> IO Errno) -> C.CSymlink
  wrapSymlink go pSource pDestination = handleAsFuseError $ do
    source <- peekCString pSource
    destination <- peekCString pDestination
    go source destination

  wrapRename :: (FilePath -> FilePath -> IO Errno) -> C.CRename
  wrapRename go pOld pNew _flags = handleAsFuseError $ do
    -- we ignore the rename flags because #define _GNU_SOURCE is needed to use the constants
    old <- peekCString pOld
    new <- peekCString pNew
    go old new

  wrapLink :: (FilePath -> FilePath -> IO Errno) -> C.CLink
  wrapLink go pSource pDestination = handleAsFuseError $ do
    source <- peekCString pSource
    destination <- peekCString pDestination
    go source destination

  wrapChmod :: (FilePath -> Maybe fh -> FileMode -> IO Errno) -> C.CChmod
  wrapChmod go pFilePath mode pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    go filePath mfh mode

  wrapChown :: (FilePath -> Maybe fh -> UserID -> GroupID -> IO Errno) -> C.CChown
  wrapChown go pFilePath uid gid pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    go filePath mfh uid gid

  wrapTruncate :: (FilePath -> Maybe fh -> FileOffset -> IO Errno) -> C.CTruncate
  wrapTruncate go pFilePath off pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    go filePath mfh off

  wrapOpen :: (FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)) -> C.COpen
  wrapOpen go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (flags :: CInt) <- (#peek struct fuse_file_info, flags) pFuseFileInfo
    let openFileFlags = OpenFileFlags
          { append   = testBitSet flags (#const O_APPEND)
          , nonBlock = testBitSet flags (#const O_NONBLOCK)
          , trunc    = testBitSet flags (#const O_TRUNC)
          , exclusive = False
          , noctty    = False
          }
        openMode
          | testBitSet flags (#const O_RDWR)   = ReadWrite
          | testBitSet flags (#const O_WRONLY) = WriteOnly
          | otherwise = ReadOnly -- O_RDONLY
    go filePath openMode openFileFlags >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapRead :: (FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)) -> C.CRead
  wrapRead go pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    go filePath fh bufSize off >>= \case
      Left errno -> pure $ Left errno
      Right bytes -> BU.unsafeUseAsCStringLen bytes $ \(pBytes, bytesLen) -> do
        let len = bytesLen `min` fromIntegral bufSize
        copyArray pBuf pBytes len
        pure $ Right $ fromIntegral len

  wrapWrite :: (FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno CInt)) -> C.CWrite
  wrapWrite go pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    buf <- B.packCStringLen (pBuf, fromIntegral bufSize)
    go filePath fh buf off

  wrapStatfs :: (String -> IO (Either Errno FileSystemStats)) -> C.CStatfs
  wrapStatfs go pStr pStatVFS = handleAsFuseError $ do
    str <- peekCString pStr
    go str >>= \case
      Left errno -> pure errno
      Right statvfs -> do
        poke pStatVFS statvfs
        pure eOK

  wrapFlush :: (FilePath -> fh -> IO Errno) -> C.CFlush
  wrapFlush go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    go filePath fh

  wrapRelease :: (FilePath -> fh -> IO ()) -> C.CRelease
  wrapRelease go pFilePath pFuseFileInfo = go' `finally` delFH pFuseFileInfo
    where
    go' = handleAsFuseError $ do
      filePath <- peekCString pFilePath
      fh <- getFH pFuseFileInfo
      go filePath fh
      pure eOK

  wrapFsync :: (FilePath -> fh -> SyncType -> IO Errno) -> C.CFsync
  wrapFsync go pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    go filePath fh (if isDataSync /= 0 then DataSync else FullSync)

  wrapOpendir :: (FilePath -> IO (Either Errno fh)) -> C.COpendir
  wrapOpendir go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapReaddir :: (FilePath -> fh -> IO (Either Errno [(FilePath, FileStat)])) -> C.CReaddir
  wrapReaddir go pFilePath pBuf pFillDir _off pFuseFileInfo _readdirFlags = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    let fillDir = mkFillDir pFillDir
        fillEntry :: (FilePath, FileStat) -> IO ()
        fillEntry (fileName, fileStat) =
          withCString fileName $ \pFileName ->
          with fileStat $ \pFileStat -> do
            _ <- fillDir pBuf pFileName pFileStat 0 0
            pure ()
    go filePath fh >>= \case
      Left errno -> pure errno
      Right entries -> do
        traverse_ fillEntry entries
        pure eOK

  wrapReleasedir :: (FilePath -> fh -> IO Errno) -> C.CReleasedir
  wrapReleasedir go pFilePath pFuseFileInfo = go' `finally` delFH pFuseFileInfo
    where
    go' = handleAsFuseError $ do
      filePath <- peekCString pFilePath
      fh <- getFH pFuseFileInfo
      go filePath fh

  wrapFsyncdir :: (FilePath -> fh -> SyncType -> IO Errno) -> C.CFsyncdir
  wrapFsyncdir go pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    go filePath fh (if isDataSync /= 0 then DataSync else FullSync)

  wrapInit :: (FuseConfig -> IO FuseConfig) -> C.CInit
  -- TODO HFuse used `defaultExceptionHandler` instead of handler
  -- TODO use parameters
  wrapInit go _fuseConnInfo pFuseConfig = do
    _ <- handle (void . handler) $ do
      -- @pFuseConfig@ is filled beforehand by fuse_opt_parse in libfuse so we pass it
      -- as-is to the callback as the default value.
      fuseConfigOld <- fromCFuseConfig <$> peek pFuseConfig
      fuseConfigNew <- go fuseConfigOld
      -- The return value of the callback is poked back to @pFuseConfig@. Note that, by
      -- doing this the fields of @fuse_config@ which we do /not/ implement are left
      -- unchanged. This is the intended behavior.
      poke pFuseConfig $ toCFuseConfig fuseConfigNew
    pure nullPtr

  wrapDestroy :: IO () -> C.CDestroy
  -- TODO HFuse used `defaultExceptionHandler` instead of handler
  wrapDestroy go _privateData = handle (void . handler) go

  wrapAccess :: (FilePath -> AccessMode -> IO Errno) -> C.CAccess
  wrapAccess go pFilePath mode = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    go filePath accessMode
    where
    accessMode
      | testBitSet mode (#const F_OK) = FileOK
      | otherwise = PermOK
          (testBitSet mode (#const R_OK))
          (testBitSet mode (#const W_OK))
          (testBitSet mode (#const X_OK))

-- | Calls @fuse_parse_cmdline@ to parse the part of the commandline arguments that
-- we care about.
--
-- @fuse_parse_cmdline@ will modify the `C.FuseArgs` struct passed in to remove those
-- arguments; the `C.FuseArgs` struct containing remaining arguments must be passed to
-- @fuse_mount@/@fuse_new@.
--
-- The multithreaded runtime will be used regardless of the threading flag!
-- See the comment in @fuse_session_exit@ for why.
fuseParseCommandLine :: Ptr C.FuseArgs -> IO (Maybe (Maybe String, Bool))
fuseParseCommandLine pArgs =
  allocaBytes (#size struct fuse_cmdline_opts) $ \pOpts -> do
    retval <- C.fuse_parse_cmdline pArgs pOpts
    if retval == 0
      then do
        mountPoint <- do
          pMountPoint <- (#peek struct fuse_cmdline_opts, mountpoint) pOpts
          if pMountPoint /= nullPtr
            then do
              a <- peekCString pMountPoint
              -- free fuse_cmdline_opts.mountpoint because it is allocated with realloc (see libfuse's examples)
              free pMountPoint
              pure $ Just a
            else pure Nothing
        foreground <- (/= (0 :: CInt)) <$> (#peek struct fuse_cmdline_opts, foreground) pOpts
        pure $ Just (mountPoint, foreground)
      else pure Nothing

-- TODO or rather, @fuse_daemonize@?
-- | Haskell version of @daemon(2)@
--
-- Mimics @daemon()@'s use of @_exit()@ instead of @exit()@; we depend on this in
-- `fuseMainReal`, because otherwise we'll unmount the filesystem when the foreground process exits.
daemon :: IO a -> IO b
daemon io = do
  _ <- forkProcess (d `catchIOError` const exitFailure)
  exitImmediately ExitSuccess
  error "This is unreachable code"
  where
  d = do
    _ <- createSession
    changeWorkingDirectory "/"
    -- need to open @/dev/null@ twice because `hDuplicateTo` can't dup a
    -- ReadWriteMode to a ReadMode handle
    withFile "/dev/null" WriteMode $ \devNullOut -> do
      hDuplicateTo devNullOut stdout
      hDuplicateTo devNullOut stderr
    withFile "/dev/null" ReadMode $ \devNullIn -> do
      hDuplicateTo devNullIn stdin
    _ <- io
    exitSuccess

-- | @withSignalHandlers handler io@ installs signal handlers while @io@ is executed.
withSignalHandlers :: IO () -> IO a -> IO a
withSignalHandlers exitHandler = bracket_ setHandlers resetHandlers
  where
  setHandlers = do
    let sigHandler = Signals.CatchOnce exitHandler
    void $ Signals.installHandler Signals.sigINT  sigHandler Nothing
    void $ Signals.installHandler Signals.sigHUP  sigHandler Nothing
    void $ Signals.installHandler Signals.sigTERM sigHandler Nothing
    void $ Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing
  resetHandlers = do
    void $ Signals.installHandler Signals.sigINT  Signals.Default Nothing
    void $ Signals.installHandler Signals.sigHUP  Signals.Default Nothing
    void $ Signals.installHandler Signals.sigTERM Signals.Default Nothing
    void $ Signals.installHandler Signals.sigPIPE Signals.Default Nothing

-- | Mounts the filesystem, forks, and then starts fuse.
fuseMainReal
  :: Bool
  -> Ptr C.StructFuse
  -> String
  -> IO a
fuseMainReal = \foreground pFuse mountPt ->
  let strategy = if foreground
        then (>>) (changeWorkingDirectory "/") . procMain
        else daemon . procMain
  in withCString mountPt $ \cMountPt -> do
       -- TODO handle failure! (return value /= 0) throw? return Left?
       _ <- C.fuse_mount pFuse cMountPt
       strategy pFuse `finally` C.fuse_unmount pFuse
  where
  -- here, we're finally inside the daemon process, we can run the main loop
  procMain pFuse = do
    session <- C.fuse_get_session pFuse
    -- calling fuse_session_exit to exit the main loop only appears to work
    -- with the multithreaded fuse loop. In the single-threaded case, FUSE
    -- depends on their recv() call to finish with EINTR when signals arrive.
    -- This doesn't happen with GHC's signal handling in place.
    -- TODO confirm this
    withSignalHandlers (C.fuse_session_exit session) $ do
      retVal <- C.fuse_loop_mt_31 pFuse 0 -- this 0 is @clone_fd@ argument TODO allow configuring this?
      if retVal == 0
        then exitSuccess
        else exitFailure

-- | Parses the commandline arguments and runs fuse.
fuseRun :: Exception e => String -> [String] -> FuseOperations fh -> (e -> IO Errno) -> IO ()
fuseRun prog args ops handler =
  catchIOError
    (withFuseArgs prog args $ \pArgs -> do
      cmd <- fuseParseCommandLine pArgs
      case cmd of
        Nothing -> fail ""
        Just (Nothing, _) -> fail "Usage error: mount point required"
        Just (Just mountPt, foreground) ->
          withCFuseOperations ops handler $ \pOp -> do
            let opSize = (#size struct fuse_operations)
                privData = nullPtr
            -- TODO fuse_new returns nullPtr on failure
            -- but it's unlikely because fuseParseCommandLine already succeeded at this point
            -- TODO dispose pFuse? (@fuse_destroy@)
            pFuse <- C.fuse_new pArgs pOp opSize privData
            fuseMainReal foreground pFuse mountPt)
    ((\errStr -> unless (null errStr) (putStrLn errStr) >> exitFailure) . ioeGetErrorString)

-- | Main function of FUSE.
--
-- This is all that has to be called from the @main@ function. On top of
-- the `FuseOperations` record with filesystem implementation, you must give
-- an exception handler converting Haskell exceptions to `Errno`.
--
-- This function does the following:
--
--   * parses command line options (@-d@, @-s@ and @-h@) ;
--
--   * passes all options after @--@ to the fusermount program ;
--
--   * mounts the filesystem by calling @fusermount@ ;
--
--   * installs signal handlers for `Signals.keyboardSignal`,
--     `Signals.lostConnection`,
--     `Signals.softwareTermination` and
--     `Signals.openEndedPipe` ;
--
--   * registers an exit handler to unmount the filesystem on program exit ;
--
--   * registers the operations ;
--
--   * calls FUSE event loop.
fuseMain :: Exception e => FuseOperations fh -> (e -> IO Errno) -> IO ()
fuseMain ops handler = do
  -- this used to be implemented using libfuse's fuse_main. Doing this will fork()
  -- from C behind the GHC runtime's back, which deadlocks in GHC 6.8.
  -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the
  -- lower-level fuse_new/fuse_loop_mt API.
  prog <- getProgName
  args <- getArgs
  fuseRun prog args ops handler

-- TODO move to another module
-- | Marshals a Haskell string into a NUL terminated C string in a locale-dependent way.
--
-- Does `withCStringLen` and copies it into the destination buffer.
--
-- If the destination buffer is not long enough to hold the source string, it is truncated and a
-- NUL byte is appended at the end of the buffer.
pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
  withCStringLen src $ \(pSrc, srcSize) -> do
    -- withCStringLen does *not* append NUL byte at the end
    let bufSize0 = bufSize - 1
    copyArray pBuf pSrc (min bufSize0 srcSize)
    pokeElemOff pBuf (min bufSize0 srcSize) 0

-- | Unwraps the newtype `Errno`.
unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

-- | `maybeWith` applied to `FunPtr`.
maybeWithFun :: (a -> (FunPtr b -> IO c) -> IO c) -> Maybe a -> (FunPtr b -> IO c) -> IO c
maybeWithFun = maybe ($ nullFunPtr)

-- | Automatically releases a @foreign import ccall "wrapper"@ with `freeHaskellFunPtr`.
withHaskellFunPtr :: (fun -> IO (FunPtr fun)) -> fun -> (FunPtr fun -> IO c) -> IO c
withHaskellFunPtr wrapper fun = bracket (wrapper fun) freeHaskellFunPtr

-- TODO move to another module?
-- | Gets a file handle from `C.FuseFileInfo` which is embedded with `newFH`.
getFH :: Ptr C.FuseFileInfo -> IO fh
getFH pFuseFileInfo = do
  sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
  deRefStablePtr $ castPtrToStablePtr sptr

-- | Embeds a file handle into `C.FuseFileInfo`. It should be freed with `delFH` when no longer
-- required.
newFH :: Ptr C.FuseFileInfo -> fh -> IO ()
newFH pFuseFileInfo fh = do
  sptr <- newStablePtr fh
  (#poke struct fuse_file_info, fh) pFuseFileInfo $ castStablePtrToPtr sptr

-- | Frees a file handle in `C.FuseFileInfo` which is embedded with `newFH`.
delFH :: Ptr C.FuseFileInfo -> IO ()
delFH pFuseFileInfo = do
  sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
  freeStablePtr $ castPtrToStablePtr sptr

-- TODO rename this
foreign import ccall "dynamic"
  mkFillDir :: FunPtr C.FuseFillDir -> C.FuseFillDir
