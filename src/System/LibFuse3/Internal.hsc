{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The core stuff
--
-- This is an internal module. It is exposed to allow fine-tuning and workarounds but its API is not stable.
module System.LibFuse3.Internal where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, bracket_, catch, finally, fromException, handle)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Foreign
  ( FunPtr
  , Ptr
  , StablePtr
  , allocaBytes
  , castPtrToStablePtr
  , castStablePtrToPtr
  , copyArray
  , deRefStablePtr
  , free
  , freeHaskellFunPtr
  , freeStablePtr
  , maybeWith
  , newStablePtr
  , nullFunPtr
  , nullPtr
  , peek
  , peekArray
  , peekByteOff
  , poke
  , pokeByteOff
  , with
  )
import Foreign.C (CInt(CInt), CString, Errno, eFAULT, eINVAL, eIO, eNOSYS, eOK, getErrno, peekCString, resetErrno, throwErrno, withCStringLen)
import GHC.IO.Handle (hDuplicateTo)
import System.Clock (TimeSpec)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.IO (IOMode(ReadMode, WriteMode), SeekMode(AbsoluteSeek, RelativeSeek, SeekFromEnd), hPutStrLn, stderr, stdin, stdout, withFile)
import System.LibFuse3.FileStat (FileStat)
import System.LibFuse3.FileSystemStats (FileSystemStats)
import System.LibFuse3.FuseConfig (FuseConfig, fromCFuseConfig, toCFuseConfig)
import System.LibFuse3.Internal.Resource (daemonizeResourceT, resMallocBytes, resNew, resNewArray, resNewCString, resNewFilePath)
import System.LibFuse3.Utils (pokeCStringLen0, testBitSet, unErrno)
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files (blockSpecialMode, characterSpecialMode, directoryMode, namedPipeMode, regularFileMode, socketMode, symbolicLinkMode)
import System.Posix.IO (OpenFileFlags, OpenMode(ReadOnly, ReadWrite, WriteOnly), defaultFileFlags)
import System.Posix.Internals (c_access, peekFilePath, withFilePath)
import System.Posix.Process (createSession)
import System.Posix.Types (ByteCount, COff(COff), CSsize, DeviceID, FileMode, FileOffset, GroupID, UserID)
import Text.Printf (hPrintf, printf)

import qualified Control.Monad.Trans.Resource as Res
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.LibFuse3.Internal.C as C
import qualified System.Posix.IO
import qualified System.Posix.Signals as Signals

#include <fuse.h>
#include <fuse_lowlevel.h>
#include <sys/xattr.h>

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

-- | Passed to `fuseSetxattr`.
data SetxattrFlag
  -- | Create a new attribute if it does not exist, or replace the value if it already exists (@0@)
  = SetxattrDefault
  -- | Perform a pure create, which fails if the named attribute exists already (@XATTR_CREATE@)
  | SetxattrCreate
  -- | Perform a pure replace operation, which fails if the named attribute does not already exist (@XATTR_REPLACE@)
  | SetxattrReplace
  deriving (Eq, Show)

-- | Tests if access permissions to the file is granted or the file exists.
--
-- Calls @access@. Compared to `System.Posix.Files.fileAccess` and
-- `System.Posix.Files.fileExist`, this function doesn't translate the errno and just
-- returns @()@ to indicate success, or throws an error to indicate failure.
access :: FilePath -> AccessMode -> IO ()
access path mode = do
  e <- accessErrno path mode
  if e == eOK
    then pure ()
    else throwErrno "access"

-- | Same as `access` but returns the `Errno` instead of throwing an exception.
--
-- Returns `eOK` on success.
accessErrno :: FilePath -> AccessMode -> IO Errno
accessErrno path mode = withFilePath path $ \cPath -> do
  let cMode = case mode of
        FileOK -> #const F_OK
        PermOK r w x ->
          (if r then (#const R_OK) else 0) .|.
          (if w then (#const W_OK) else 0) .|.
          (if x then (#const X_OK) else 0)
  resetErrno
  ret <- c_access cPath cMode
  if ret == 0
    then pure eOK
    else getErrno

-- memo: when adding a new field, make sure to update resCFuseOperations
-- | The file system operations.
--
-- All operations are optional. Each field is named against @struct fuse_operations@ in
-- @fuse.h@.
--
-- @fh@ is the file handle type returned by `fuseOpen`, and subsequently passed to all
-- other file operations.
--
-- @dh@ is the directory handle type returned by `fuseOpendir`, and subsequently passed to
-- `fuseReaddir` and `fuseReleasedir`.
data FuseOperations fh dh = FuseOperations
  { -- | Implements 'System.Posix.Files.getSymbolicLinkStatus' operation (POSIX @lstat(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open.
    fuseGetattr :: Maybe (FilePath -> Maybe fh -> IO (Either Errno FileStat))

  , -- | Implements 'System.Posix.Files.readSymbolicLink' operation (POSIX @readlink(2)@).
    --
    -- This function should not append a terminating NUL byte. The returned 'FilePath'
    -- might be truncated depending on caller buffer size.
    fuseReadlink :: Maybe (FilePath -> IO (Either Errno FilePath))

  , -- | Implements 'System.Posix.Files.createDevice' (POSIX @mknod(2)@).
    --
    -- This function will also be called for regular file creation if `fuseCreate` is not defined.
    --
    -- `fileModeToEntryType` is handy to pattern match on the request type of the node.
    fuseMknod :: Maybe (FilePath -> FileMode -> DeviceID -> IO Errno)

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
    -- @Nothing@ even if it is open.
    --
    -- Unless @FUSE_CAP_HANDLE_KILLPRIV@ is disabled, this method is expected to reset the
    -- setuid and setgid bits.
    fuseChown :: Maybe (FilePath -> Maybe fh -> UserID -> GroupID -> IO Errno)

  , -- | Implements 'System.Posix.Files.setFileSize' (POSIX @truncate(2)@).
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open.
    --
    -- Unless @FUSE_CAP_HANDLE_KILLPRIV@ is disabled, this method is expected to reset the
    -- setuid and setgid bits.
    fuseTruncate :: Maybe (FilePath -> Maybe fh -> FileOffset -> IO Errno)

  , -- | Implements 'System.Posix.Files.openFd' (POSIX @open(2)@).  On success, returns
    -- 'Right' of a filehandle-like value that will be passed to future file operations; on
    -- failure, returns 'Left' of the appropriate 'Errno'.
    --
    --   * Creation flags will be filtered out / handled by the kernel.
    --   * Access modes should be used by this to check if the operation is permitted.
    --   * The filesystem is expected to properly handle the @O_APPEND@ flag and ensure that
    --     each write is appending to the end of the file.
    --   * If this method returns @Left `eNOSYS`@ and @FUSE_CAP_NO_OPEN_SUPPORT@ is set in
    --     @fuse_conn_info.capable@, this is treated as success and future calls to open
    --     will also succeed without being sent to the filesystem process.
    --
    -- TODO allow this method to set @fuse_file_info.direct_io@ and @fuse_file_info.keep_cache@
    fuseOpen :: Maybe (FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh))

  , -- | Implements Unix98 @pread(2)@.
    --
    -- It differs from 'System.Posix.Files.fdRead' by the explicit 'FileOffset' argument.
    fuseRead :: Maybe (FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString))

  , -- | Implements Unix98 @pwrite(2)@.
    --
    -- It differs from 'System.Posix.Files.fdWrite' by the explicit 'FileOffset' argument.
    --
    -- Unless @FUSE_CAP_HANDLE_KILLPRIV@ is disabled, this method is expected to reset the
    -- setuid and setgid bits.
    fuseWrite :: Maybe (FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno CInt))

  , -- | Implements @statfs(2)@.
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

  , -- | Implements @setxattr(2)@.
    --
    -- The parameters are: path, name, value and flags.
    fuseSetxattr :: Maybe (FilePath -> String -> B.ByteString -> SetxattrFlag -> IO Errno)

  , -- | Implements @getxattr(2)@.
    --
    -- The parameters are path and name.
    fuseGetxattr :: Maybe (FilePath -> String -> IO (Either Errno B.ByteString))

  , -- | Implements @listxattr(2)@.
    fuseListxattr :: Maybe (FilePath -> IO (Either Errno [String]))

  , -- | Implements @removexattr(2)@.
    fuseRemovexattr :: Maybe (FilePath -> String -> IO Errno)

  , -- | Implements @opendir(3)@.
    --
    -- This method should check if the open operation is permitted for this directory.
    fuseOpendir :: Maybe (FilePath -> IO (Either Errno dh))

  , -- | Implements @readdir(3)@.
    --
    -- The entire contents of the directory should be returned as a list of tuples
    -- (corresponding to the first mode of operation documented in @fuse.h@).
    --
    -- The returned list should contain entries of \".\" and \"..\".
    --
    -- Each element of the list is a pair of the name and the stat. The name should not
    -- include the path to it. The implementation may return @Nothing@ as the stat; in this
    -- case `fuseGetattr` is called instead.
    fuseReaddir :: Maybe (FilePath -> dh -> IO (Either Errno [(String, Maybe FileStat)]))

  , -- | Implements @closedir(3)@.
    fuseReleasedir :: Maybe (FilePath -> dh -> IO Errno)

  , -- | Synchronize the directory's contents; analogous to `fuseFsync`.
    fuseFsyncdir :: Maybe (FilePath -> dh -> SyncType -> IO Errno)

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

  , -- | Implements 'System.Posix.Files.openFd' (POSIX @open(2)@). Creates and opens a regular
    -- file.
    --
    -- If this is not implemented, `fuseMknod` and `fuseOpen` methods will be called instead.
    --
    -- See `fuseOpen` for notes on the flags.
    fuseCreate :: Maybe (FilePath -> OpenMode -> FileMode -> OpenFileFlags -> IO (Either Errno fh))

    -- TODO , fuseLock :: _

  , -- | Implements @utimensat(2)@.
    --
    -- Changes the access and modification times of a file with nanosecond resolution.
    --
    -- @fh@ will always be @Nothing@ if the file is not currently open, but may also be
    -- @Nothing@ even if it is open.
    fuseUtimens :: Maybe (FilePath -> Maybe fh -> TimeSpec -> TimeSpec -> IO Errno)

    -- TODO , fuseBmap :: _
    -- TODO , fuseIoctl :: _
    -- TODO , fusePoll :: _
    -- TODO , fuseWriteBuf :: _
    -- TODO , fuseReadBuf :: _
    -- TODO , fuseFlock :: _

  , -- | Implements 'System.Posix.Fcntl.fileAllocate' (@posix_fallocate(3)@). Allocates
    -- space for an open file.
    fuseFallocate :: Maybe (FilePath -> fh -> CInt -> FileOffset -> FileOffset -> IO Errno)

  , -- | Implements @copy_file_range(2)@.
    fuseCopyFileRange :: Maybe (FilePath -> fh -> FileOffset -> FilePath -> fh -> FileOffset -> ByteCount -> CInt -> IO (Either Errno CSsize))

  , -- | Implements 'System.Posix.IO.fdSeek' @lseek(3)@.
    --
    -- /Note:/ This is silently ignored if libfuse doesn't support @lseek@ operation (requires libfuse-3.8.0).
    fuseLseek :: Maybe (FilePath -> fh -> FileOffset -> SeekMode -> IO (Either Errno FileOffset))
  }

-- | An empty set of operations whose fields are @Nothing@.
defaultFuseOperations :: FuseOperations fh dh
defaultFuseOperations = FuseOperations Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Merges two `FuseOperations` in a left-biased manner.
mergeLFuseOperations :: FuseOperations fh dh -> FuseOperations fh dh -> FuseOperations fh dh
mergeLFuseOperations
  (FuseOperations a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35)
  (FuseOperations b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35)
  = FuseOperations (a1 <|> b1) (a2 <|> b2) (a3 <|> b3) (a4 <|> b4) (a5 <|> b5) (a6 <|> b6) (a7 <|> b7) (a8 <|> b8) (a9 <|> b9) (a10 <|> b10) (a11 <|> b11) (a12 <|> b12) (a13 <|> b13) (a14 <|> b14) (a15 <|> b15) (a16 <|> b16) (a17 <|> b17) (a18 <|> b18) (a19 <|> b19) (a20 <|> b20) (a21 <|> b21) (a22 <|> b22) (a23 <|> b23) (a24 <|> b24) (a25 <|> b25) (a26 <|> b26) (a27 <|> b27) (a28 <|> b28) (a29 <|> b29) (a30 <|> b30) (a31 <|> b31) (a32 <|> b32) (a33 <|> b33) (a34 <|> b34) (a35 <|> b35)

-- | Allocates a @fuse_operations@ struct and pokes `FuseOperations` into it.
--
-- Each field of `FuseOperations` is converted into a C function pointer and is assigned
-- to a corresponding field of @struct fuse_operations@.
--
-- The created `C.FuseOperations` has the following invariants:
--
--   - The content of @fuse_file_info.fh@ is a Haskell value of type @StablePtr fh@ or
--     @StablePtr dh@, depending on operations. It is created with `newFH`, accessed with
--     `getFH` and released with `delFH`.
--
--   - Every methods handle Haskell exception with the supplied error handler. Any exceptions
--     not catched by it are catched, logged and returns `eIO`. This means that `exitSuccess`
--     /does not work/ inside `FuseOperations`.
--
--   - NULL filepaths (passed from libfuse if `FuseConfig.nullpathOk` is set) are
--     translated to empty strings.
resCFuseOperations
  :: forall fh dh e
   . Exception e
  => FuseOperations fh dh
  -> ExceptionHandler e
  -> ResourceT IO (Ptr C.FuseOperations)
resCFuseOperations ops handlerRaw = do
  fuseGetattr       <- resC C.mkGetattr       wrapGetattr       (fuseGetattr ops)
  fuseReadlink      <- resC C.mkReadlink      wrapReadlink      (fuseReadlink ops)
  fuseMknod         <- resC C.mkMknod         wrapMknod         (fuseMknod ops)
  fuseMkdir         <- resC C.mkMkdir         wrapMkdir         (fuseMkdir ops)
  fuseUnlink        <- resC C.mkUnlink        wrapUnlink        (fuseUnlink ops)
  fuseRmdir         <- resC C.mkRmdir         wrapRmdir         (fuseRmdir ops)
  fuseSymlink       <- resC C.mkSymlink       wrapSymlink       (fuseSymlink ops)
  fuseRename        <- resC C.mkRename        wrapRename        (fuseRename ops)
  fuseLink          <- resC C.mkLink          wrapLink          (fuseLink ops)
  fuseChmod         <- resC C.mkChmod         wrapChmod         (fuseChmod ops)
  fuseChown         <- resC C.mkChown         wrapChown         (fuseChown ops)
  fuseTruncate      <- resC C.mkTruncate      wrapTruncate      (fuseTruncate ops)
  fuseOpen          <- resC C.mkOpen          wrapOpen          (fuseOpen ops)
  fuseRead          <- resC C.mkRead          wrapRead          (fuseRead ops)
  fuseWrite         <- resC C.mkWrite         wrapWrite         (fuseWrite ops)
  fuseStatfs        <- resC C.mkStatfs        wrapStatfs        (fuseStatfs ops)
  fuseFlush         <- resC C.mkFlush         wrapFlush         (fuseFlush ops)
  fuseRelease       <- resC C.mkRelease       wrapRelease       (fuseRelease ops)
  fuseFsync         <- resC C.mkFsync         wrapFsync         (fuseFsync ops)
  fuseSetxattr      <- resC C.mkSetxattr      wrapSetxattr      (fuseSetxattr ops)
  fuseGetxattr      <- resC C.mkGetxattr      wrapGetxattr      (fuseGetxattr ops)
  fuseListxattr     <- resC C.mkListxattr     wrapListxattr     (fuseListxattr ops)
  fuseRemovexattr   <- resC C.mkRemovexattr   wrapRemovexattr   (fuseRemovexattr ops)
  fuseOpendir       <- resC C.mkOpendir       wrapOpendir       (fuseOpendir ops)
  fuseReaddir       <- resC C.mkReaddir       wrapReaddir       (fuseReaddir ops)
  fuseReleasedir    <- resC C.mkReleasedir    wrapReleasedir    (fuseReleasedir ops)
  fuseFsyncdir      <- resC C.mkFsyncdir      wrapFsyncdir      (fuseFsyncdir ops)
  fuseInit          <- resC C.mkInit          wrapInit          (fuseInit ops)
  fuseDestroy       <- resC C.mkDestroy       wrapDestroy       (fuseDestroy ops)
  fuseAccess        <- resC C.mkAccess        wrapAccess        (fuseAccess ops)
  fuseCreate        <- resC C.mkCreate        wrapCreate        (fuseCreate ops)
  fuseUtimens       <- resC C.mkUtimens       wrapUtimens       (fuseUtimens ops)
  fuseFallocate     <- resC C.mkFallocate     wrapFallocate     (fuseFallocate ops)
  fuseCopyFileRange <- resC C.mkCopyFileRange wrapCopyFileRange (fuseCopyFileRange ops)
  fuseLseek         <- resC C.mkLseek         wrapLseek         (fuseLseek ops)
  fmap snd $ resNew C.FuseOperations
    -- not (yet) implemented methods
    { fuseLock = nullFunPtr
    , fuseBmap = nullFunPtr
    , fuseIoctl = nullFunPtr
    , fusePoll = nullFunPtr
    , fuseWriteBuf = nullFunPtr
    , fuseReadBuf = nullFunPtr
    , fuseFlock = nullFunPtr
    , ..
    }
  where
  -- wraps the supplied handler to make sure no Haskell exceptions are propagated to the C land
  handler :: ExceptionHandler SomeException
  handler se = case fromException se of
    Nothing -> defaultExceptionHandler se
    Just e -> handlerRaw e `catch` defaultExceptionHandler

  -- convert a Haskell function to C one with @wrapMeth@, get its @FunPtr@, and associate it with freeHaskellFunPtr
  resC :: (cfunc -> IO (FunPtr cfunc)) -> (hsfunc -> cfunc) -> Maybe hsfunc -> ResourceT IO (FunPtr cfunc)
  resC _ _ Nothing = pure nullFunPtr
  resC mkMeth wrapMeth (Just hsfunc) = fmap snd $ Res.allocate (mkMeth $ wrapMeth hsfunc) freeHaskellFunPtr

  -- return negated errno as specified by fuse.h. also handle any Haskell exceptions
  handleAsFuseError :: IO Errno -> IO CInt
  handleAsFuseError = handleAsFuseErrorResult . fmap Left -- assumes eOK == 0

  -- return a (successful) result as positive int and a negated errno as negative int
  handleAsFuseErrorResult :: IO (Either Errno CInt) -> IO CInt
  handleAsFuseErrorResult = handleAsFuseErrorIntegral

  handleAsFuseErrorCSsize :: IO (Either Errno CSsize) -> IO CSsize
  handleAsFuseErrorCSsize = handleAsFuseErrorIntegral

  handleAsFuseErrorCOff :: IO (Either Errno COff) -> IO COff
  handleAsFuseErrorCOff = handleAsFuseErrorIntegral

  handleAsFuseErrorIntegral :: Integral a => IO (Either Errno a) -> IO a
  handleAsFuseErrorIntegral = fmap (either (fromIntegral . negate . unErrno) id) . handle (fmap Left . handler)

  peekFilePathOrEmpty :: CString -> IO FilePath
  peekFilePathOrEmpty pFilePath
    | pFilePath == nullPtr = pure ""
    | otherwise            = peekFilePath pFilePath

  peekOpenFileFlagsAndMode :: Ptr C.FuseFileInfo -> IO (OpenFileFlags, OpenMode)
  peekOpenFileFlagsAndMode pFuseFileInfo = do
    (flags :: CInt) <- (#peek struct fuse_file_info, flags) pFuseFileInfo
    let openFileFlags = defaultFileFlags
          { System.Posix.IO.append   = testBitSet flags (#const O_APPEND)
          , System.Posix.IO.nonBlock = testBitSet flags (#const O_NONBLOCK)
          , System.Posix.IO.trunc    = testBitSet flags (#const O_TRUNC)
          }
        openMode
          | testBitSet flags (#const O_RDWR)   = ReadWrite
          | testBitSet flags (#const O_WRONLY) = WriteOnly
          | otherwise = ReadOnly -- O_RDONLY
    pure (openFileFlags, openMode)

  wrapGetattr :: (FilePath -> Maybe fh -> IO (Either Errno FileStat)) -> C.CGetattr
  wrapGetattr go pFilePath pStat pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    mfh <- getFH pFuseFileInfo
    go filePath mfh >>= \case
      Left errno -> pure errno
      Right stat -> do
        poke pStat stat
        pure eOK

  wrapReadlink :: (FilePath -> IO (Either Errno FilePath)) -> C.CReadlink
  wrapReadlink go pFilePath pBuf bufSize = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath >>= \case
      Left errno -> pure errno
      Right target -> do
        -- This will truncate target if it's longer than the buffer can hold,
        -- as specified by fuse.h
        pokeCStringLen0 (pBuf, (fromIntegral bufSize)) target
        pure eOK

  wrapMknod :: (FilePath -> FileMode -> DeviceID -> IO Errno) -> C.CMknod
  wrapMknod go pFilePath mode dev = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath mode dev

  wrapMkdir :: (FilePath -> FileMode -> IO Errno) -> C.CMkdir
  wrapMkdir go pFilePath mode = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath mode

  wrapUnlink :: (FilePath -> IO Errno) -> C.CUnlink
  wrapUnlink go pFilePath = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath

  wrapRmdir :: (FilePath -> IO Errno) -> C.CRmdir
  wrapRmdir go pFilePath = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath

  wrapSymlink :: (FilePath -> FilePath -> IO Errno) -> C.CSymlink
  wrapSymlink go pSource pDestination = handleAsFuseError $ do
    source <- peekFilePath pSource
    destination <- peekFilePath pDestination
    go source destination

  wrapRename :: (FilePath -> FilePath -> IO Errno) -> C.CRename
  wrapRename go pOld pNew _flags = handleAsFuseError $ do
    -- we ignore the rename flags because #define _GNU_SOURCE is needed to use the constants
    -- TODO return EINVAL if flags are specified?
    old <- peekFilePath pOld
    new <- peekFilePath pNew
    go old new

  wrapLink :: (FilePath -> FilePath -> IO Errno) -> C.CLink
  wrapLink go pSource pDestination = handleAsFuseError $ do
    source <- peekFilePath pSource
    destination <- peekFilePath pDestination
    go source destination

  wrapChmod :: (FilePath -> Maybe fh -> FileMode -> IO Errno) -> C.CChmod
  wrapChmod go pFilePath mode pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    mfh <- getFH pFuseFileInfo
    go filePath mfh mode

  wrapChown :: (FilePath -> Maybe fh -> UserID -> GroupID -> IO Errno) -> C.CChown
  wrapChown go pFilePath uid gid pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    mfh <- getFH pFuseFileInfo
    go filePath mfh uid gid

  wrapTruncate :: (FilePath -> Maybe fh -> FileOffset -> IO Errno) -> C.CTruncate
  wrapTruncate go pFilePath off pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    mfh <- getFH pFuseFileInfo
    go filePath mfh off

  wrapOpen :: (FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)) -> C.COpen
  wrapOpen go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    (openFileFlags, openMode) <- peekOpenFileFlagsAndMode pFuseFileInfo
    go filePath openMode openFileFlags >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapRead :: (FilePath -> fh -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)) -> C.CRead
  wrapRead go pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekFilePathOrEmpty pFilePath
    fh <- getFHJust pFuseFileInfo
    go filePath fh bufSize off >>= \case
      Left errno -> pure $ Left errno
      Right bytes -> BU.unsafeUseAsCStringLen bytes $ \(pBytes, bytesLen) -> do
        let len = bytesLen `min` fromIntegral bufSize
        copyArray pBuf pBytes len
        pure $ Right $ fromIntegral len

  wrapWrite :: (FilePath -> fh -> B.ByteString -> FileOffset -> IO (Either Errno CInt)) -> C.CWrite
  wrapWrite go pFilePath pBuf bufSize off pFuseFileInfo = handleAsFuseErrorResult $ do
    filePath <- peekFilePathOrEmpty pFilePath
    fh <- getFHJust pFuseFileInfo
    buf <- BU.unsafePackCStringLen (pBuf, fromIntegral bufSize)
    go filePath fh buf off

  wrapStatfs :: (String -> IO (Either Errno FileSystemStats)) -> C.CStatfs
  wrapStatfs go pStr pStatVFS = handleAsFuseError $ do
    str <- peekFilePath pStr
    go str >>= \case
      Left errno -> pure errno
      Right statvfs -> do
        poke pStatVFS statvfs
        pure eOK

  wrapFlush :: (FilePath -> fh -> IO Errno) -> C.CFlush
  wrapFlush go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    fh <- getFHJust pFuseFileInfo
    go filePath fh

  wrapRelease :: (FilePath -> fh -> IO ()) -> C.CRelease
  wrapRelease go pFilePath pFuseFileInfo = go' `finally` delFH pFuseFileInfo
    where
    go' = handleAsFuseError $ do
      filePath <- peekFilePathOrEmpty pFilePath
      fh <- getFHJust pFuseFileInfo
      go filePath fh
      pure eOK

  wrapFsync :: (FilePath -> fh -> SyncType -> IO Errno) -> C.CFsync
  wrapFsync go pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    fh <- getFHJust pFuseFileInfo
    go filePath fh (if isDataSync /= 0 then DataSync else FullSync)

  wrapSetxattr :: (FilePath -> String -> B.ByteString -> SetxattrFlag -> IO Errno) -> C.CSetxattr
  wrapSetxattr go pFilePath pName pValue valueSize cFlags = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    name <- peekCString pName
    value <- BU.unsafePackCStringLen (pValue, fromIntegral valueSize)
    let eflag = case cFlags of
          0 -> Right SetxattrDefault
          (#const XATTR_CREATE) -> Right SetxattrCreate
          (#const XATTR_REPLACE) -> Right SetxattrReplace
          _ -> Left eINVAL
    either pure (go filePath name value) eflag

  wrapGetxattr :: (FilePath -> String -> IO (Either Errno B.ByteString)) -> C.CGetxattr
  wrapGetxattr go pFilePath pName pValueBuf bufSize = handleAsFuseErrorResult $ do
    filePath <- peekFilePath pFilePath
    name <- peekCString pName
    go filePath name >>= \case
      Left errno -> pure $ Left errno
      Right bytes
        | bufSize == 0 -> pure $ Right $ fromIntegral $ B.length bytes
        | otherwise -> BU.unsafeUseAsCStringLen bytes $ \(pBytes, bytesLen) -> do
            let len = bytesLen `min` fromIntegral bufSize
            copyArray pValueBuf pBytes len
            pure $ Right $ fromIntegral len

  wrapListxattr :: (FilePath -> IO (Either Errno [String])) -> C.CListxattr
  wrapListxattr go pFilePath pBuf bufSize = handleAsFuseErrorResult $ do
    filePath <- peekFilePath pFilePath
    go filePath >>= \case
      Left errno -> pure $ Left errno
      Right names -> withCStringLen (concatMap (<> "\0") names) $ \(pNames, namesLen) ->
        if bufSize == 0
          then pure $ Right $ fromIntegral namesLen
          else do
            let len = namesLen `min` fromIntegral bufSize
            copyArray pBuf pNames len
            pure $ Right $ fromIntegral len

  wrapRemovexattr :: (FilePath -> String -> IO Errno) -> C.CRemovexattr
  wrapRemovexattr go pFilePath pName = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    name <- peekCString pName
    go filePath name

  wrapOpendir :: (FilePath -> IO (Either Errno dh)) -> C.COpendir
  wrapOpendir go pFilePath pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath >>= \case
      Left errno -> pure errno
      Right dh -> do
        newFH pFuseFileInfo dh
        pure eOK

  wrapReaddir :: (FilePath -> dh -> IO (Either Errno [(String, Maybe FileStat)])) -> C.CReaddir
  wrapReaddir go pFilePath pBuf pFillDir _off pFuseFileInfo _readdirFlags = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    dh <- getFHJust pFuseFileInfo
    let fillDir = peekFuseFillDir pFillDir
        fillEntry :: (FilePath, Maybe FileStat) -> IO ()
        fillEntry (fileName, fileStat) =
          withFilePath fileName $ \pFileName ->
          maybeWith with fileStat $ \pFileStat -> do
            _ <- fillDir pBuf pFileName pFileStat 0 0
            pure ()
    go filePath dh >>= \case
      Left errno -> pure errno
      Right entries -> do
        traverse_ fillEntry entries
        pure eOK

  wrapReleasedir :: (FilePath -> dh -> IO Errno) -> C.CReleasedir
  wrapReleasedir go pFilePath pFuseFileInfo = go' `finally` delFH pFuseFileInfo
    where
    go' = handleAsFuseError $ do
      filePath <- peekFilePathOrEmpty pFilePath
      dh <- getFHJust pFuseFileInfo
      go filePath dh

  wrapFsyncdir :: (FilePath -> dh -> SyncType -> IO Errno) -> C.CFsyncdir
  wrapFsyncdir go pFilePath isDataSync pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    dh <- getFHJust pFuseFileInfo
    go filePath dh (if isDataSync /= 0 then DataSync else FullSync)

  wrapInit :: (FuseConfig -> IO FuseConfig) -> C.CInit
  -- TODO implement read/write of fuseConnInfo; watch out for read-only fields
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
  wrapDestroy go _privateData = handle (void . handler) go

  wrapAccess :: (FilePath -> AccessMode -> IO Errno) -> C.CAccess
  wrapAccess go pFilePath mode = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    go filePath accessMode
    where
    accessMode
      | testBitSet mode (#const F_OK) = FileOK
      | otherwise = PermOK
          (testBitSet mode (#const R_OK))
          (testBitSet mode (#const W_OK))
          (testBitSet mode (#const X_OK))

  wrapCreate :: (FilePath -> OpenMode -> FileMode -> OpenFileFlags -> IO (Either Errno fh)) -> C.CCreate
  wrapCreate go pFilePath mode pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    (openFileFlags, openMode) <- peekOpenFileFlagsAndMode pFuseFileInfo
    go filePath openMode mode openFileFlags >>= \case
      Left errno -> pure errno
      Right fh -> do
        newFH pFuseFileInfo fh
        pure eOK

  wrapUtimens :: (FilePath -> Maybe fh -> TimeSpec -> TimeSpec -> IO Errno) -> C.CUtimens
  wrapUtimens go pFilePath arrTs pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePathOrEmpty pFilePath
    mfh <- getFH pFuseFileInfo
    [atime, mtime] <- peekArray 2 arrTs
    go filePath mfh atime mtime

  wrapFallocate :: (FilePath -> fh -> CInt -> FileOffset -> FileOffset -> IO Errno) -> C.CFallocate
  wrapFallocate go pFilePath mode offset len pFuseFileInfo = handleAsFuseError $ do
    filePath <- peekFilePath pFilePath
    fh <- getFHJust pFuseFileInfo
    go filePath fh mode offset len

  wrapCopyFileRange :: (FilePath -> fh -> FileOffset -> FilePath -> fh -> FileOffset -> ByteCount -> CInt -> IO (Either Errno CSsize)) -> C.CCopyFileRange
  wrapCopyFileRange go pFilePathIn pFuseFileInfoIn offsetIn pFilePathOut pFuseFileInfoOut offsetOut size flags = handleAsFuseErrorCSsize $ do
    filePathIn <- peekFilePath pFilePathIn
    fhIn <- getFHJust pFuseFileInfoIn
    filePathOut <- peekFilePath pFilePathOut
    fhOut <- getFHJust pFuseFileInfoOut
    go filePathIn fhIn offsetIn filePathOut fhOut offsetOut size flags

  wrapLseek :: (FilePath -> fh -> FileOffset -> SeekMode -> IO (Either Errno FileOffset)) -> C.CLseek
  wrapLseek go pFilePath offset whence pFuseFileInfo = handleAsFuseErrorCOff $ do
    filePath <- peekFilePath pFilePath
    fh <- getFHJust pFuseFileInfo
    let emode = case whence of
          (#const SEEK_SET) -> Right AbsoluteSeek
          (#const SEEK_CUR) -> Right RelativeSeek
          (#const SEEK_END) -> Right SeekFromEnd
          _ -> Left eINVAL
    either (pure . Left) (go filePath fh offset) emode

  _dummyToSuppressWarnings :: StablePtr a
  _dummyToSuppressWarnings = error "dummy" eNOSYS

-- | Allocates a @fuse_args@ struct to hold commandline arguments.
resFuseArgs :: String -> [String] -> ResourceT IO (Ptr C.FuseArgs)
resFuseArgs prog args = do
  let allArgs = (prog:args)
      argc = length allArgs
  cArgs <- traverse (fmap snd . resNewCString) allArgs
  pArgv <- fmap snd $ resNewArray cArgs
  -- call FUSE_ARGS_INIT instead?
  fuseArgs <- fmap snd $ resMallocBytes (#size struct fuse_args)
  liftIO $ do
    (#poke struct fuse_args, argc) fuseArgs argc
    (#poke struct fuse_args, argv) fuseArgs pArgv
    (#poke struct fuse_args, allocated) fuseArgs (0::CInt)
  _ <- Res.register $ C.fuse_opt_free_args fuseArgs
  pure fuseArgs

-- | Calls @fuse_parse_cmdline@ to parse the part of the commandline arguments that
-- we care about.
--
-- @fuse_parse_cmdline@ will modify the `C.FuseArgs` struct passed in to remove those
-- arguments; the `C.FuseArgs` struct containing remaining arguments must be passed to
-- @fuse_mount@/@fuse_new@.
--
-- The multithreaded runtime will be used regardless of the threading flag!
-- See the comment in @fuse_session_exit@ for why.
fuseParseCommandLine :: Ptr C.FuseArgs -> IO (Either ExitCode FuseMainArgs)
fuseParseCommandLine pArgs =
  allocaBytes (#size struct fuse_cmdline_opts) $ \pOpts -> do
    retval <- C.fuse_parse_cmdline pArgs pOpts
    if retval /= 0
      -- fuse_parse_cmdline prints an error message
      then pure $ Left $ ExitFailure 1
      else go pOpts
  where
  go pOpts = do
    pMountPoint <- (#peek struct fuse_cmdline_opts, mountpoint) pOpts
    showHelp    <- (/= (0 :: CInt)) <$> (#peek struct fuse_cmdline_opts, show_help) pOpts
    showVersion <- (/= (0 :: CInt)) <$> (#peek struct fuse_cmdline_opts, show_version) pOpts
    -- free fuse_cmdline_opts.mountpoint because it is allocated with realloc (see libfuse's examples)
    let freeMountPoint = free pMountPoint
    flip finally freeMountPoint $ case () of
      _ | showHelp -> do
            printf "usage: %s [options] <mountpoint>\n\n" =<< getProgName
            C.fuse_cmdline_help
            C.fuse_lib_help pArgs
            pure $ Left ExitSuccess
        | showVersion -> do
            ver <- peekCString =<< C.fuse_pkgversion
            printf "FUSE library version %s\n" ver
            C.fuse_lowlevel_version
            pure $ Left ExitSuccess
        | pMountPoint == nullPtr -> do
            progName <- getProgName
            hPrintf stderr "usage: %s [options] <mountpoint>\n" progName
            hPrintf stderr "       %s --help\n" progName
            pure $ Left $ ExitFailure 1
        | otherwise -> do
            mountPoint <- peekFilePath pMountPoint
            foreground <- (/= (0 :: CInt)) <$> (#peek struct fuse_cmdline_opts, foreground) pOpts
            cloneFd <- (#peek struct fuse_cmdline_opts, clone_fd) pOpts
            pure $ Right (foreground, mountPoint, cloneFd)

-- | Parses the commandline arguments and exit if the args are bad or certain informational
-- flag(s) are specified. See `fuseParseCommandLine`.
fuseParseCommandLineOrExit :: Ptr C.FuseArgs -> IO FuseMainArgs
fuseParseCommandLineOrExit pArgs = either exitWith pure =<< fuseParseCommandLine pArgs

-- | Haskell version of @fuse_daemonize@.
--
-- During the fork, transfers all of the resources in `ResourceT` (and its cleanup actions)
-- to the forked process.
--
-- Mimics @daemon()@'s use of @_exit()@ instead of @exit()@; we depend on this in
-- `fuseMainReal`, because otherwise we'll unmount the filesystem when the foreground
-- process exits.
fuseDaemonize :: ResourceT IO a -> ResourceT IO b
fuseDaemonize job = daemonizeResourceT $ do
  liftIO $ do
    _ <- createSession
    changeWorkingDirectory "/"
    -- need to open @/dev/null@ twice because `hDuplicateTo` can't dup a
    -- ReadWriteMode to a ReadMode handle
    withFile "/dev/null" WriteMode $ \devNullOut -> do
      hDuplicateTo devNullOut stdout
      hDuplicateTo devNullOut stderr
    withFile "/dev/null" ReadMode $ \devNullIn -> do
      hDuplicateTo devNullIn stdin
  _ <- job
  liftIO $ exitSuccess

-- | @withSignalHandlers handler io@ installs signal handlers while @io@ is executed.
withSignalHandlers :: IO () -> IO a -> IO a
withSignalHandlers exitHandler = bracket_ setHandlers resetHandlers
  where
  setHandlers = do
    let sigHandler = Signals.Catch exitHandler
    void $ Signals.installHandler Signals.sigINT  sigHandler Nothing
    void $ Signals.installHandler Signals.sigHUP  sigHandler Nothing
    void $ Signals.installHandler Signals.sigTERM sigHandler Nothing
    void $ Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing
  resetHandlers = do
    void $ Signals.installHandler Signals.sigINT  Signals.Default Nothing
    void $ Signals.installHandler Signals.sigHUP  Signals.Default Nothing
    void $ Signals.installHandler Signals.sigTERM Signals.Default Nothing
    void $ Signals.installHandler Signals.sigPIPE Signals.Default Nothing

-- | The parts of @fuse_parse_cmdline@ we are interested in. Passed to `fuseMainReal`.
--
-- @(foreground, mountpoint, clone_fd)@
--
-- So far, we don't interpret the value of @clone_fd@ at all so its type is `CInt`.
type FuseMainArgs = (Bool, String, CInt)

-- | Mounts the filesystem, forks (if requested), and then starts fuse.
fuseMainReal
  :: Ptr C.StructFuse
  -> FuseMainArgs
  -> ResourceT IO a
fuseMainReal = \pFuse (foreground, mountPt, cloneFd) -> do
  let run = if foreground
        then \io -> liftIO $ changeWorkingDirectory "/" >> io
        else fuseDaemonize . liftIO
  cMountPt <- fmap snd $ resNewFilePath mountPt
  mountResult <- snd <$> Res.allocate (C.fuse_mount pFuse cMountPt) (\_ -> C.fuse_unmount pFuse)
  if mountResult == 0
    then run $ procMain pFuse cloneFd
    else liftIO $ fail "fuse_mount failed"
  where
  -- here, we're finally inside the daemon process, we can run the main loop
  procMain pFuse cloneFd = do
    session <- C.fuse_get_session pFuse
    -- Due to some interaction between GHC runtime, calling fuse_session_exit once doesn't
    -- stop fuse_loop_mt_31. On receiving a second signal the loop exits and the filesystem
    -- is unmounted.
    -- Adding the RTS option @--install-signal-handlers=no@ does not fix the issue.
    --
    -- On the other hand, @fusermount3 -u@ successfully unmounts the filesystem on the first
    -- attempt.
    withSignalHandlers (C.fuse_session_exit session) $ do
      retVal <- C.fuse_loop_mt_31 pFuse cloneFd
      if retVal == 0
        then exitSuccess
        else exitFailure

-- | Parses the commandline arguments and runs fuse.
fuseRun :: Exception e => String -> [String] -> FuseOperations fh dh -> ExceptionHandler e -> IO a
fuseRun prog args ops handler = runResourceT $ do
  pArgs <- resFuseArgs prog args
  mainArgs <- liftIO $ fuseParseCommandLineOrExit pArgs
  pOp <- resCFuseOperations ops handler
  pFuse <- fmap snd $ Res.allocate
    (C.fuse_new pArgs pOp (#size struct fuse_operations) nullPtr)
    (\p -> unless (p == nullPtr) $ C.fuse_destroy p)
  if pFuse == nullPtr
    then liftIO exitFailure -- fuse_new prints an error message
    else fuseMainReal pFuse mainArgs

-- | Main function of FUSE.
--
-- This is all that has to be called from the @main@ function. On top of
-- the `FuseOperations` record with filesystem implementation, you must give
-- an exception handler converting Haskell exceptions to `Errno`.
fuseMain :: Exception e => FuseOperations fh dh -> ExceptionHandler e -> IO ()
fuseMain ops handler = do
  -- this used to be implemented using libfuse's fuse_main. Doing this will fork()
  -- from C behind the GHC runtime's back, which deadlocks in GHC 6.8.
  -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the
  -- lower-level fuse_new/fuse_loop_mt API.
  prog <- getProgName
  args <- getArgs
  fuseRun prog args ops handler

-- | An exception handler which converts Haskell exceptions from `FuseOperations` methods to `Errno`.
type ExceptionHandler e = e -> IO Errno

-- | Catches any exception, logs it to stderr, and returns `eIO`.
--
-- Suitable as a default exception handler.
--
-- __NOTE 1__ This differs from the one in the @HFuse@ package which returns `eFAULT`.
--
-- __NOTE 2__ If the filesystem is daemonized (as default), the exceptions will not be logged because
-- stderr is redirected to @\/dev\/null@.
defaultExceptionHandler :: ExceptionHandler SomeException
defaultExceptionHandler e = hPutStrLn stderr (show e) >> pure eIO
  where
  _dummyToSuppressWarnings = error "dummy" eFAULT

-- | Gets a file handle from `C.FuseFileInfo` which is embedded with `newFH`.
--
-- If either the @Ptr `C.FuseFileInfo`@ itself or its @fh@ field is NULL, returns @Nothing@.
getFH :: Ptr C.FuseFileInfo -> IO (Maybe fh)
getFH pFuseFileInfo
  | pFuseFileInfo == nullPtr = pure Nothing
  | otherwise = do
    sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
    -- Note that this implementation relies on the fact that @fuse_file_info.fh@ is
    -- @NULL@-initialized before @fuse_operations.open@ and @.opendir@, and remains @NULL@
    -- if they are unimplemented. It's a hack but we check this because because if we
    -- didn't, we'll hit undefined behavior.
    if sptr == nullPtr
      then pure Nothing
      else fmap Just $ deRefStablePtr $ castPtrToStablePtr sptr

-- | Gets a file handle from `C.FuseFileInfo`.
--
-- @
-- getFHJust = fmap fromJust . `getFH`
-- @
--
-- This means you must make sure that `getFH` returns @Just@ or you'll get a Haskell
-- exception. /However/, it's deliberately made lazy so that calling `getFHJust` itself
-- won't throw but trying to use the returned value will.
--
-- This function is implemented this way in order to take care of rare(?) cases in which
-- `fuseRead`\/`fuseReaddir` is implemented but not `fuseOpen`\/`fuseOpendir` resp. In
-- such a case, `newFH` would not be called but only `getFH` would be. Without some
-- protection, we would be dereferencing a non-initialized `StablePtr`, which is
-- /undefined behavior/. Throwing a Haskell exception in a pure code is much better than
-- UB. See the comment in the source of `getFH` if you are interested in more explanation.
getFHJust :: Ptr C.FuseFileInfo -> IO fh
getFHJust = fmap fromJust . getFH

-- | Embeds a file handle into `C.FuseFileInfo`. It should be freed with `delFH` when no
-- longer required.
newFH :: Ptr C.FuseFileInfo -> fh -> IO ()
newFH pFuseFileInfo fh = do
  sptr <- newStablePtr fh
  (#poke struct fuse_file_info, fh) pFuseFileInfo $ castStablePtrToPtr sptr

-- | Frees a file handle in `C.FuseFileInfo` which is embedded with `newFH`.
delFH :: Ptr C.FuseFileInfo -> IO ()
delFH pFuseFileInfo = do
  sptr <- (#peek struct fuse_file_info, fh) pFuseFileInfo
  -- if sptr is NULL, it should mean newFH have not called. See getFH and getFHJust for
  -- more info
  unless (sptr == nullPtr) $
    freeStablePtr $ castPtrToStablePtr sptr

-- | Materializes the callback of @readdir@ to marshal `fuseReaddir`.
foreign import ccall "dynamic"
  peekFuseFillDir :: FunPtr C.FuseFillDir -> C.FuseFillDir
