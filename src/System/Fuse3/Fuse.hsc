{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Based on `System.Fuse` in the package "HFuse-0.2.5.0".

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
module System.Fuse3.Fuse where
-- TODO rename to System.Fuse3.Internal

import Control.Exception (Exception, bracket, bracket_, finally, handle)
import Control.Monad (unless, void)
import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import FileStat (FileStat)
import Foreign
  ( FunPtr
  , Ptr
  , alloca
  , allocaBytes
  , callocBytes
  , castPtrToStablePtr
  , castStablePtrToPtr
  , copyArray
  , deRefStablePtr
  , free
  , freeStablePtr
  , maybePeek
  , newStablePtr
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
import Foreign.C (CInt(CInt), CSize(CSize), CString, CStringLen, CUInt(CUInt), Errno(Errno), eOK, peekCString, withCString, withCStringLen)
import GHC.IO.Handle (hDuplicateTo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess), exitFailure, exitWith)
import System.IO (IOMode(ReadMode, WriteMode), stderr, stdin, stdout, withFile)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.IO (OpenFileFlags(OpenFileFlags), OpenMode(ReadOnly, ReadWrite, WriteOnly))
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.Types (ByteCount, CDev(CDev), CGid(CGid), CMode(CMode), COff(COff), CUid(CUid), DeviceID, Fd, FileMode, FileOffset, GroupID, UserID)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.Fuse3.Internal.C as C
import qualified System.Posix.IO
import qualified System.Posix.Signals as Signals

#include <fuse.h>

-- TODO
data EntryType

fileModeToEntryType :: FileMode -> EntryType
fileModeToEntryType = _

-- TODO
data RenameFlags

cuintToRenameFlags :: CUInt -> RenameFlags
cuintToRenameFlags = _

-- TODO
data SyncType

toSyncType :: CInt -> SyncType
toSyncType = _

-- TODO
data FileSystemStats

pokeFileSystemStats :: Ptr C.StatVFS -> FileSystemStats -> IO ()
pokeFileSystemStats = _

-- TODO the @mode@ parameter for @access(2)@
-- F_OK or (R_OK | W_OK | X_OK)
data AccessMode

toAccessMode :: CInt -> AccessMode
toAccessMode = _

-- TODO change the types of each field to @Maybe (foo -> bar -> IO baz)@
-- TODO add low-level FuseOperations whose members are @FunPtr foo@
-- memo: when adding a new field, make sure to update withCFuseOperations
data FuseOperations fh = FuseOperations
  -- | Implements 'System.Posix.Files.getSymbolicLinkStatus' operation (POSIX @lstat(2)@).
  --
  -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
  -- @Nothing@ even if it is open.
  { fuseGetFileStat :: FilePath -> Maybe fh -> IO (Either Errno FileStat)

  -- | Implements 'System.Posix.Files.readSymbolicLink' operation (POSIX @readlink(2)@).
  --
  -- The returned 'FilePath' might be truncated depending on caller buffer size.
  , fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)

  -- | Implements 'System.Posix.Files.createDevice' (POSIX @mknod(2)@).
  --
  -- This function will also be called for regular file creation if 'fuseCreate' (TODO
  -- name pending) is not defined.
  , fuseCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno

  -- | Implements 'System.Posix.Directory.createDirectory' (POSIX @mkdir(2)@).
  , fuseCreateDirectory :: FilePath -> FileMode -> IO Errno

  -- | Implements 'System.Posix.Files.removeLink' (POSIX @unlink(2)@).
  , fuseRemoveLink :: FilePath -> IO Errno

  -- | Implements 'Ststen.Posix.Directory.removeDirectory' (POSIX @rmdir(2)@).
  , fuseRemoveDirectory :: FilePath -> IO Errno

  -- | Implements 'System.Posix.Files.createSymbolicLink' (POSIX @symlink(2)@).
  , fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno

  -- | Implements 'System.Posix.Files.rename' (POSIX @rename(2)@). TODO describe the flags
  , fuseRename :: FilePath -> FilePath -> RenameFlags -> IO Errno

  -- | Implements 'System.Posix.Files.createLink' (POSIX @link(2)@).
  , fuseCreateLink :: FilePath -> FilePath -> IO Errno

  -- | Implements 'System.Posix.Files.setFileMode' (POSIX @chmod(2)@).
  --
  -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
  -- @Nothing@ even if it is open.
  , fuseSetFileMode :: FilePath -> Maybe fh -> FileMode -> IO Errno

  -- | Implements 'System.Posix.Files.setOwnerAndGroup' (POSIX @chown(2)@).
  --
  -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
  -- @Nothing@ even if it is open. This method is expected to reset the setuid and setgid
  -- bits.
  --
  -- TODO FUSE_CAP_HANDLE_KILLPRIV?
  , fuseSetOwnerAndGroup :: FilePath -> Maybe fh -> UserID -> GroupID -> IO Errno

  -- | Implements 'System.Posix.Files.setFileSize' (POSIX @truncate(2)@).
  --
  -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
  -- @Nothing@ even if it is open. This method is expected to reset the setuid and setgid
  -- bits.
  --
  -- TODO FUSE_CAP_HANDLE_KILLPRIV?
  , fuseSetFileSize :: FilePath -> Maybe fh -> FileOffset -> IO Errno

  -- | Implements 'System.Posix.Files.openFd' (POSIX @open(2)@).  On success, returns
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
  , fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)

  -- | Implements Unix98 @pread(2)@.
  --
  -- It differs from 'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
  , fuseRead :: FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)

  -- | Implements Unix98 @pwrite(2)@.
  --
  -- It differs from 'System.Posix.Files.fdWrite' by the explicit 'FileOffset' argument.
  --
  -- This method is expected to reset the setuid and setgid bits.
  --
  -- TODO FUSE_CAP_HANDLE_KILLPRIV?
  , fuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno CInt)

  -- | Implements @statfs(2)@. TODO describe ignored fields
  , fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)

  -- | Called when @close(2)@ has been called on an open file.
  --
  -- Note: this does not mean that the file is released.  This function may be called more
  -- than once for each @open(2)@.  The return value is passed on to the @close(2)@ system
  -- call.
  , fuseFlush :: FilePath -> fh -> IO Errno

  -- | Called when an open file has all file descriptors closed and all memory mappings
  -- unmapped.
  --
  -- For every @open@ call there will be exactly one @release@ call with the same flags.
  -- It is possible to have a file opened more than once, in which case only the last
  -- release will mean that no more reads or writes will happen on the file.
  , fuseRelease :: FilePath -> fh -> IO ()

  -- | Implements @fsync(2)@.
  , fuseSynchronizeFile :: FilePath -> fh -> SyncType -> IO Errno

  -- TODO , setxattr :: _
  -- TODO , getxattr :: _
  -- TODO , listxattr :: _
  -- TODO , removexattr :: _

  -- | Implements @opendir(3)@.
  --
  -- This method should check if the open operation is permitted for this directory.
  , fuseOpenDirectory :: FilePath -> IO (Either Errno fh)

  -- | Implements @readdir(3)@.
  --
  -- The entire contents of the directory should be returned as a list of tuples
  -- (corresponding to the first mode of operation documented in @fuse.h@).
  , fuseReadDirectory :: FilePath -> fh -> IO (Either Errno [(FilePath, FileStat)])

  -- | Implements @closedir(3)@.
  , fuseReleaseDirectory :: FilePath -> fh -> IO Errno

  -- | Synchronize the directory's contents; analogous to 'fuseSynchronizeFile'.
  , fuseSynchronizeDirectory :: FilePath -> fh -> SyncType -> IO Errno

  -- | Initializes the filesystem.  This is called before all other operations.
  , fuseInit :: IO ()

  -- | Called on filesystem exit to allow cleanup.
  , fuseDestroy :: IO ()

  -- | Implements 'System.Posix.Files.fileAccess' and 'System.Posix.Files.fileExist
  -- (POSIX @access(2)@).
  --
  -- Checks file access permissions; this will be called for the @access()@ system call.
  --
  -- If the @default_permissions@ mount option is given, this method is not called. This
  -- method is also not called under Linux kernel versions 2.4.x
  --
  -- TODO add notes about @default_permissions@ to other relevant handlers
  , fuseAccess :: FilePath -> AccessMode -> IO Errno

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

-- Allocates a fuse_args struct to hold the commandline arguments.
withFuseArgs :: String -> [String] -> (Ptr C.FuseArgs -> IO b) -> IO b
withFuseArgs prog args f = do
  let allArgs = (prog:args)
      argc = length allArgs
  withMany withCString allArgs $ \cArgs ->
    withArray cArgs $ \pArgv ->
      -- TODO call FUSE_ARGS_INIT instead?
      allocaBytes (#size struct fuse_args) $ \fuseArgs -> do
        (#poke struct fuse_args, argc) fuseArgs argc
        (#poke struct fuse_args, argv) fuseArgs pArgv
        (#poke struct fuse_args, allocated) fuseArgs (0::CInt)
        f fuseArgs `finally` C.fuse_opt_free_args fuseArgs

-- memo: a replacement of withStructFuse
withCFuseOperations
  :: Exception e
  => FuseOperations fh
  -> (e -> IO Errno)
  -> (Ptr C.FuseOperations -> IO b)
  -> IO b
withCFuseOperations ops handler cont =
  bracket (callocBytes (#size struct fuse_operations)) free $ \pOps -> do
    -- TODO freeHaskellFunPtr to *every* mk functions
    mkGetattr    wrapGetattr    >>= (#poke struct fuse_operations, getattr)    pOps
    mkReadlink   wrapReadlink   >>= (#poke struct fuse_operations, readlink)   pOps
    mkMknod      wrapMknod      >>= (#poke struct fuse_operations, mknod)      pOps
    mkMkdir      wrapMkdir      >>= (#poke struct fuse_operations, mkdir)      pOps
    mkUnlink     wrapUnlink     >>= (#poke struct fuse_operations, unlink)     pOps
    mkRmdir      wrapRmdir      >>= (#poke struct fuse_operations, rmdir)      pOps
    mkSymlink    wrapSymlink    >>= (#poke struct fuse_operations, symlink)    pOps
    mkRename     wrapRename     >>= (#poke struct fuse_operations, rename)     pOps
    mkLink       wrapLink       >>= (#poke struct fuse_operations, link)       pOps
    mkChmod      wrapChmod      >>= (#poke struct fuse_operations, chmod)      pOps
    mkChown      wrapChown      >>= (#poke struct fuse_operations, chown)      pOps
    mkTruncate   wrapTruncate   >>= (#poke struct fuse_operations, truncate)   pOps
    mkOpen       wrapOpen       >>= (#poke struct fuse_operations, open)       pOps
    mkRead       wrapRead       >>= (#poke struct fuse_operations, read)       pOps
    mkWrite      wrapWrite      >>= (#poke struct fuse_operations, write)      pOps
    mkStatfs     wrapStatfs     >>= (#poke struct fuse_operations, statfs)     pOps
    mkFlush      wrapFlush      >>= (#poke struct fuse_operations, flush)      pOps
    mkRelease    wrapRelease    >>= (#poke struct fuse_operations, release)    pOps
    mkFsync      wrapFsync      >>= (#poke struct fuse_operations, fsync)      pOps
    mkOpendir    wrapOpendir    >>= (#poke struct fuse_operations, opendir)    pOps
    mkReaddir    wrapReaddir    >>= (#poke struct fuse_operations, readdir)    pOps
    mkReleasedir wrapReleasedir >>= (#poke struct fuse_operations, releasedir) pOps
    mkFsyncdir   wrapFsyncdir   >>= (#poke struct fuse_operations, fsyncdir)   pOps
    mkInit       wrapInit       >>= (#poke struct fuse_operations, init)       pOps
    mkDestroy    wrapDestroy    >>= (#poke struct fuse_operations, destroy)    pOps
    mkAccess     wrapAccess     >>= (#poke struct fuse_operations, access)     pOps
    cont pOps
  where
  -- return negated errno as specified by fuse.h. also handle any Haskell exceptions
  handleAsFuseError :: IO Errno -> IO CInt
  handleAsFuseError = handleAsFuseErrorResult . fmap Left -- assumes eOK == 0

  -- return a (successful) result as positive int and a negated errno as negative int
  handleAsFuseErrorResult :: IO (Either Errno CInt) -> IO CInt
  handleAsFuseErrorResult = fmap (either (negate . unErrno) id) . handle (fmap Left . handler)

  wrapGetattr :: CGetattr
  wrapGetattr pFilePath pStat pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    (fuseGetFileStat ops) filePath mfh >>= \case
      Left errno -> pure errno
      Right stat -> do
        poke pStat stat
        pure eOK

  wrapReadlink :: CReadlink
  wrapReadlink pFilePath pBuf bufSize = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseReadSymbolicLink ops) filePath >>= \case
      Left errno -> pure errno
      Right target -> do
        -- This will truncate target if it's longer than the buffer can hold,
        -- as specified by fuse.h
        pokeCStringLen0 (pBuf, (fromIntegral bufSize)) target
        pure eOK

  wrapMknod :: CMknod
  wrapMknod pFilePath mode dev = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseCreateDevice ops) filePath (fileModeToEntryType mode) mode dev

  wrapMkdir :: CMkdir
  wrapMkdir pFilePath mode = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseCreateDirectory ops) filePath mode

  wrapUnlink :: CUnlink
  wrapUnlink pFilePath = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseRemoveLink ops) filePath

  wrapRmdir :: CRmdir
  wrapRmdir pFilePath = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseRemoveDirectory ops) filePath

  wrapSymlink :: CSymlink
  wrapSymlink pSource pDestination = handleAsFuseError $ do
    source <- peekCString pSource
    destination <- peekCString pDestination
    (fuseCreateSymbolicLink ops) source destination

  wrapRename :: CRename
  wrapRename pOld pNew flags = handleAsFuseError $ do
    old <- peekCString pOld
    new <- peekCString pNew
    (fuseRename ops) old new (cuintToRenameFlags flags)

  wrapLink :: CLink
  wrapLink pSource pDestination = handleAsFuseError $ do
    source <- peekCString pSource
    destination <- peekCString pDestination
    (fuseCreateLink ops) source destination

  wrapChmod :: CChmod
  wrapChmod pFilePath mode pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    (fuseSetFileMode ops) filePath mfh mode

  wrapChown :: CChown
  wrapChown pFilePath uid gid pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    (fuseSetOwnerAndGroup ops) filePath mfh uid gid

  wrapTruncate :: CTruncate
  wrapTruncate pFilePath off pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    mfh <- maybePeek getFH pFuseFileInfo
    (fuseSetFileSize ops) filePath mfh off

  wrapOpen :: COpen
  wrapOpen pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (flags :: CInt) <- (#peek struct fuse_file_info, flags) pFuseFileInfo
    let openFileFlags = OpenFileFlags
          { append   = (#const O_APPEND)   .&. flags == (#const O_APPEND)
          , nonBlock = (#const O_NONBLOCK) .&. flags == (#const O_NONBLOCK)
          , trunc    = (#const O_TRUNC)    .&. flags == (#const O_TRUNC)
          , exclusive = False
          , noctty    = False
          }
        openMode
          | (#const O_RDWR)   .&. flags == (#const O_RDWR)   = ReadWrite
          | (#const O_WRONLY) .&. flags == (#const O_WRONLY) = WriteOnly
          | otherwise = ReadOnly -- O_RDONLY
    (fuseOpen ops) filePath openMode openFileFlags >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapRead :: CRead
  wrapRead pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    (fuseRead ops) filePath fh bufSize off >>= \case
      Left errno -> pure $ Left errno
      Right bytes -> BU.unsafeUseAsCStringLen bytes $ \(pBytes, bytesLen) -> do
        let len = bytesLen `min` fromIntegral bufSize
        copyArray pBuf pBytes len
        pure $ Right $ fromIntegral len

  wrapWrite :: CWrite
  wrapWrite pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    buf <- B.packCStringLen (pBuf, fromIntegral bufSize)
    (fuseWrite ops) filePath fh buf off

  wrapStatfs :: CStatfs
  wrapStatfs pStr pStatVFS = handleAsFuseError $ do
    str <- peekCString pStr
    (fuseGetFileSystemStats ops) str >>= \case
      Left errno -> pure errno
      Right stat -> do
        pokeFileSystemStats pStatVFS stat
        pure eOK

  wrapFlush :: CFlush
  wrapFlush pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    (fuseFlush ops) filePath fh

  wrapRelease :: CRelease
  wrapRelease pFilePath pFuseFileInfo = go `finally` delFH pFuseFileInfo
    where
    go = handleAsFuseError $ do
      filePath <- peekCString pFilePath
      fh <- getFH pFuseFileInfo
      (fuseRelease ops) filePath fh
      pure eOK

  wrapFsync :: CFsync
  wrapFsync pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    (fuseSynchronizeFile ops) filePath fh (toSyncType isDataSync)

  wrapOpendir :: COpendir
  wrapOpendir pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseOpenDirectory ops) filePath >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapReaddir :: CReaddir
  wrapReaddir pFilePath pBuf pFillDir _off pFuseFileInfo _readdirFlags = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    let fillDir = mkFillDir pFillDir
        fillEntry :: (FilePath, FileStat) -> IO ()
        fillEntry (fileName, fileStat) =
          withCString fileName $ \pFileName ->
          with fileStat $ \pFileStat -> do
            _ <- fillDir pBuf pFileName pFileStat 0 0
            pure ()
    (fuseReadDirectory ops) filePath fh >>= \case
      Left errno -> pure errno
      Right entries -> do
        traverse_ fillEntry entries
        pure eOK

  wrapReleasedir :: CReleasedir
  wrapReleasedir pFilePath pFuseFileInfo = go `finally` delFH pFuseFileInfo
    where
    go = handleAsFuseError $ do
      filePath <- peekCString pFilePath
      fh <- getFH pFuseFileInfo
      (fuseReleaseDirectory ops) filePath fh

  wrapFsyncdir :: CFsyncdir
  wrapFsyncdir pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    fh <- getFH pFuseFileInfo
    (fuseSynchronizeDirectory ops) filePath fh (toSyncType isDataSync)

  wrapInit :: CInit
  -- TODO HFuse used `defaultExceptionHandler` instead of handler
  -- TODO use parameters
  wrapInit _fuseConnInfo _fuseConfig = do
    _ <- handle (void . handler) (fuseInit ops)
    pure nullPtr

  wrapDestroy :: CDestroy
  -- TODO HFuse used `defaultExceptionHandler` instead of handler
  wrapDestroy _privateData = handle (void . handler) (fuseDestroy ops)

  wrapAccess :: CAccess
  wrapAccess pFilePath mode = handleAsFuseError $ do
    filePath <- peekCString pFilePath
    (fuseAccess ops) filePath (toAccessMode mode)

-- Calls @fuse_parse_cmdline@ to parse the part of the commandline arguments that
-- we care about. @fuse_parse_cmdline@ will modify the `C.FuseArgs` struct passed in
-- to remove those arguments; the `C.FuseArgs` struct containing remaining arguments
-- must be passed to @fuse_mount@/@fuse_new@.
--
-- The multithreaded runtime will be used regardless of the threading flag!
-- See the comment in @fuse_session_exit@ for why.
--
-- TODO the second part of tuple may be unused
fuseParseCommandLine :: Ptr C.FuseArgs -> IO (Maybe (Maybe String, Bool, Bool))
fuseParseCommandLine pArgs =
  alloca $ \pMountPt ->
  alloca $ \pMultiThreaded ->
  alloca $ \pFG -> do
    -- TODO do we need to poke here?
    poke pMultiThreaded 0
    poke pFG 0
    retval <- C.fuse_parse_cmdline pArgs pMountPt pMultiThreaded pFG
    if retval == 0
      then do
        cMountPt <- peek pMountPt
        mountPt <- if cMountPt /= nullPtr
          then do
            a <- peekCString cMountPt
            -- TODO why free?
            free cMountPt
            pure $ Just a
          else pure Nothing
        multiThreaded <- peek pMultiThreaded
        foreground <- peek pFG
        pure $ Just (mountPt, multiThreaded == 1, foreground == 1)
      else pure Nothing

-- Haskell version of @daemon(2)@
--
-- Mimics @daemon()@'s use of @_exit()@ instead of @exit()@; we depend on this in
-- `fuseMainRealP, because otherwise we'll unmount the filesystem when the foreground process exits.
daemon :: IO a -> IO b
-- `exitImmediately` never returns. This `error` is only here to please the
-- typechecker.
-- It's a dirty hack, but I think the problem is in the posix package, not
-- making this @IO a@ instead of @IO ()@
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
    exitWith ExitSuccess

-- Installs signal handlers for the duration of the main loop.
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

-- TODO do we need `C.FuseBuf`?
handleOnce :: Ptr C.FuseSession -> Ptr C.FuseBuf -> Ptr C.FuseChan -> IO ()
handleOnce session buf chan = do
  size <- C.fuse_chan_bufsize chan
  allocaBytes (fromIntegral size) $ \ptr -> do
    #{poke struct fuse_buf, mem}  buf ptr
    #{poke struct fuse_buf, size} buf size
    with chan $ \chanP -> do
      C.fuse_session_receive_buf session buf chanP
      C.fuse_session_process_buf session buf =<< peek chanP

forAllChans
  :: Ptr C.FuseSession
  -> (Ptr C.FuseChan -> IO a -> IO a)
  -> IO a
  -> IO a
forAllChans session fun cont = go nullPtr
  where
  go cur = do
    new <- C.fuse_session_next_chan session cur
    if new == nullPtr
      then cont
      else fun new $ go new

-- TODO: Add an unregister function to run as well
runInline
  :: (Fd -> IO () -> IO b)
  -> (b -> IO ())
  -> (Either String () -> IO a) -- TODO change to (IO a)
  -> Ptr C.StructFuse
  -> IO a
runInline register unregister act pFuse = bracket
  (callocBytes #{size struct fuse_buf}) free $ \buf -> do
    session <- C.fuse_get_session pFuse
    let registerChan chan cont = do
          fd <- C.fuse_chan_fd chan
          bracket
            (register fd (handleOnce session buf chan))
            unregister
            (const cont)
    ret <- forAllChans session registerChan $ withSignalHandlers (C.fuse_session_exit session) $ act $ Right ()
    C.fuse_session_exit session
    pure ret

-- Mounts the filesystem, forks, and then starts fuse
fuseMainReal
  :: Maybe (Fd -> IO () -> IO b, b -> IO (), Either String () -> IO a)
  -> Bool
  -> Ptr C.StructFuse
  -> String
  -> IO a
fuseMainReal = \inline foreground pFuse mountPt ->
  let strategy = case inline of
        Just (register, unregister, act) -> runInline register unregister act
        Nothing -> if foreground
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
    withSignalHandlers (C.fuse_session_exit session) $ do
      retVal <- C.fuse_loop_mt pFuse
      if retVal == 1
        then exitWith ExitSuccess
        else exitFailure

fuseRun :: Exception e => String -> [String] -> FuseOperations fh -> (e -> IO Errno) -> IO ()
fuseRun prog args ops handler =
  catchIOError
    (withFuseArgs prog args $ \pArgs ->
       do cmd <- fuseParseCommandLine pArgs
          case cmd of
            Nothing -> fail ""
            Just (Nothing, _, _) -> fail "Usage error: mount point required"
            Just (Just mountPt, _, foreground) ->
              withCFuseOperations ops handler $ \pOp -> do
                let opSize = (#size struct fuse_operations)
                    privData = _later
                -- TODO fuse_new returns nullPtr on failure
                -- but it's unlikely because fuseParseCommandLine already succeeded at this point
                -- TODO dispose pFuse? (@fuse_destroy@)
                pFuse <- C.fuse_new pArgs pOp opSize privData
                fuseMainReal Nothing foreground pFuse mountPt)
    ((\errStr -> unless (null errStr) (putStrLn errStr) >> exitFailure) . ioeGetErrorString)

-- | Main function of FUSE.
--
-- This is all that has to be called from the @main@ function. On top of
-- the `FuseOperations` record with filesystem implementation, you muset give
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
pokeCStringLen :: CStringLen -> String -> IO ()
pokeCStringLen (pBuf, bufSize) src =
  withCStringLen src $ \(pSrc, srcSize) ->
    copyArray pBuf pSrc (min bufSize srcSize)

pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
  withCStringLen src $ \(pSrc, srcSize) -> do
    -- withCStringLen does *not* append NUL byte at the end
    let bufSize0 = bufSize - 1
    copyArray pBuf pSrc (min bufSize0 srcSize)
    pokeElemOff pBuf (min bufSize0 srcSize) 0

unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

-- TODO move to another module?
-- Get filehandle
getFH :: Ptr C.FuseFileInfo -> IO fh
getFH pFuseFileInfo = do
  sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
  deRefStablePtr $ castPtrToStablePtr sptr

newFH :: Ptr C.FuseFileInfo -> fh -> IO ()
newFH pFuseFileInfo fh = do
  sptr <- newStablePtr fh
  (#poke struct fuse_file_info, fh) pFuseFileInfo $ castStablePtrToPtr sptr

delFH :: Ptr C.FuseFileInfo -> IO ()
delFH pFuseFileInfo = do
  sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
  freeStablePtr $ castPtrToStablePtr sptr

-- TODO move to another module (along with withCFuseOperations?)
-- TODO @Char -> IO ()@ are placeholders
foreign import ccall "dynamic"
  mkFillDir :: FunPtr C.FuseFillDir -> C.FuseFillDir

type CGetattr = CString -> Ptr FileStat -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkGetattr :: CGetattr -> IO (FunPtr CGetattr)

type CReadlink = CString -> CString -> CSize -> IO CInt
foreign import ccall "wrapper"
  mkReadlink :: CReadlink -> IO (FunPtr CReadlink)

type CMknod = CString -> CMode -> CDev -> IO CInt
foreign import ccall "wrapper"
  mkMknod :: CMknod -> IO (FunPtr CMknod)

type CMkdir = CString -> CMode -> IO CInt
foreign import ccall "wrapper"
  mkMkdir :: CMkdir -> IO (FunPtr CMkdir)

type CUnlink = CString -> IO CInt
foreign import ccall "wrapper"
  mkUnlink :: CUnlink -> IO (FunPtr CUnlink)

type CRmdir = CString -> IO CInt
foreign import ccall "wrapper"
  mkRmdir :: CRmdir -> IO (FunPtr CRmdir)

type CSymlink = CString -> CString -> IO CInt
foreign import ccall "wrapper"
  mkSymlink :: CSymlink -> IO (FunPtr CSymlink)

type CRename = CString -> CString -> CUInt -> IO CInt
foreign import ccall "wrapper"
  mkRename :: CRename -> IO (FunPtr CRename)

type CLink = CString -> CString -> IO CInt
foreign import ccall "wrapper"
  mkLink :: CLink -> IO (FunPtr CLink)

type CChmod = CString -> CMode -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkChmod :: CChmod -> IO (FunPtr CChmod)

type CChown = CString -> CUid -> CGid -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkChown :: CChown -> IO (FunPtr CChown)

type CTruncate = CString -> COff -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkTruncate :: CTruncate -> IO (FunPtr CTruncate)

type COpen = CString -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkOpen :: COpen -> IO (FunPtr COpen)

type CRead = CString -> CString -> CSize -> COff -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkRead :: CRead -> IO (FunPtr CRead)

type CWrite = CString -> CString -> CSize -> COff -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkWrite :: CWrite -> IO (FunPtr CWrite)

type CStatfs = CString -> Ptr C.StatVFS -> IO CInt
foreign import ccall "wrapper"
  mkStatfs :: CStatfs -> IO (FunPtr CStatfs)

type CFlush = CString -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFlush :: CFlush -> IO (FunPtr CFlush)

type CRelease = CString -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkRelease :: CRelease -> IO (FunPtr CRelease)

type CFsync = CString -> CInt -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFsync :: CFsync -> IO (FunPtr CFsync)

type COpendir = CString -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkOpendir :: COpendir -> IO (FunPtr COpendir)

type CReaddir = CString -> Ptr C.FuseFillDirBuf -> FunPtr C.FuseFillDir -> COff -> Ptr C.FuseFileInfo -> C.FuseReaddirFlags -> IO CInt
foreign import ccall "wrapper"
  mkReaddir :: CReaddir -> IO (FunPtr CReaddir)

type CReleasedir = CString -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkReleasedir :: CReleasedir -> IO (FunPtr CReleasedir)

type CFsyncdir = CString -> CInt -> Ptr C.FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFsyncdir :: CFsyncdir -> IO (FunPtr CFsyncdir)

type CInit = Ptr C.FuseConnInfo -> Ptr C.FuseConfig -> IO (Ptr ())
foreign import ccall "wrapper"
  mkInit :: CInit -> IO (FunPtr CInit)

type CDestroy = Ptr () -> IO ()
foreign import ccall "wrapper"
  mkDestroy :: CDestroy -> IO (FunPtr CDestroy)

type CAccess = CString -> CInt -> IO CInt
foreign import ccall "wrapper"
  mkAccess :: CAccess -> IO (FunPtr CAccess)
