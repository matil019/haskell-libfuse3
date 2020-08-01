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

import Control.Exception (Exception, bracket, bracket_, finally)
import Control.Monad (unless, void)
import FileStat (FileStat)
import Foreign (Ptr, alloca, allocaBytes, callocBytes, free, nullPtr, peek, poke, pokeByteOff, with, withArray, withMany)
import Foreign.C (CInt, Errno, peekCString, withCString)
import GHC.IO.Handle (hDuplicateTo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess), exitFailure, exitWith)
import System.IO (IOMode(ReadMode, WriteMode), stderr, stdin, stdout, withFile)
import System.IO.Error (catchIOError, ioeGetErrorString)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.IO (OpenFileFlags, OpenMode)
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.Types (ByteCount, DeviceID, Fd, FileMode, FileOffset, GroupID, UserID)

import qualified Data.ByteString as B
import qualified System.Fuse3.Internal.C as C
import qualified System.Posix.Signals as Signals

#include <fuse.h>

-- TODO
data RenameFlags

-- TODO
data EntryType

-- TODO
data SyncType

-- TODO
data FileSystemStats

-- TODO change the types of each field to @Maybe (foo -> bar -> IO baz)@
-- TODO add low-level FuseOperations whose members are @FunPtr foo@
data FuseOperations fh = FuseOperations
  -- | Implements 'System.Posix.Files.getSymbolicLinkStatus' operation (POSIX @lstat(2)@).
  { fuseGetFileStat :: FilePath -> IO (Either Errno FileStat)

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
  , fuseWrite :: FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)

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

  -- | Check file access permissions; this will be called for the @access()@ system call.
  --
  -- If the @default_permissions@ mount option is given, this method is not called. This
  -- method is also not called under Linux kernel versions 2.4.x
  --
  -- TODO add notes about @default_permissions@ to other relevant handlers
  , fuseAccess :: FilePath -> Int -> IO Errno  -- TODO what is this Int?

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
  bracket (callocBytes (#size struct fuse_operations)) free $ \pOp -> do
    _

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
