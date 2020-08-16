{-# LANGUAGE RecordWildCards #-}
-- | C land.
--
-- This is an internal module. It is exposed to allow fine-tuning and workarounds but its API is not stable.
--
-- Exported C called from Haskell
module System.LibFuse3.Internal.C where

import Data.Word (Word32, Word64)
import Foreign (FunPtr, Ptr, Storable, peekByteOff, pokeByteOff)
import Foreign.C (CDouble, CInt(CInt), CSize(CSize), CString, CUInt(CUInt))
import System.Clock (TimeSpec)
import System.LibFuse3.FileStat (FileStat)
import System.LibFuse3.FileSystemStats (FileSystemStats)
import System.Posix.Types (CDev(CDev), CGid(CGid), CMode(CMode), COff(COff), CSsize(CSsize), CUid(CUid))

import qualified Foreign
import qualified System.Posix.Internals as Posix

#include <fuse.h>

-- | @struct fuse_args@
data FuseArgs

-- | @struct fuse_buf@
data FuseBuf

-- | @struct fuse_bufvec@
data FuseBufvec

-- | @struct fuse_cmdline_opts@
data FuseCmdlineOpts

-- | The direct, storable representation of @struct fuse_config@.
--
-- Not to be confused with the high-level `System.LibFuse3.Internal.FuseConfig`.
data FuseConfig = FuseConfig
  { -- | @set_gid@
    setGid :: CInt
  , -- | @gid@
    gid :: CUInt
  , -- | @set_uid@
    setUid :: CInt
  , -- | @uid@
    uid :: CUInt
  , -- | @set_mode@
    setMode :: CInt
  , -- | @umask@
    umask :: CUInt
  , -- | @entry_timeout@
    entryTimeout :: CDouble
  , -- | @negative_timeout@
    negativeTimeout :: CDouble
  , -- | @attr_timeout@
    attrTimeout :: CDouble
  , -- | @intr@
    intr :: CInt
  , -- | @intr_signal@
    intrSignal :: CInt
  , -- | @remember@
    remember :: CInt
  , -- | @hard_remove@
    hardRemove :: CInt
  , -- | @use_ino@
    useIno :: CInt
  , -- | @readdir_ino@
    readdirIno :: CInt
  , -- | @direct_io@
    directIo :: CInt
  , -- | @kernel_cache@
    kernelCache :: CInt
  , -- | @auto_cache@
    autoCache :: CInt
  , -- | @ac_attr_timeout_set@
    acAttrTimeoutSet :: CInt
  , -- | @ac_attr_timeout@
    acAttrTimeout :: CDouble
  , -- | @nullpath_ok@
    nullpathOk :: CInt
  }
  deriving (Eq, Show)

-- | Targets @struct fuse_config@.
instance Storable FuseConfig where
  sizeOf _ = #size struct fuse_config

  alignment _ = #alignment struct fuse_config

  peek ptr = do
    setGid           <- (#peek struct fuse_config, set_gid)             ptr
    gid              <- (#peek struct fuse_config, gid)                 ptr
    setUid           <- (#peek struct fuse_config, set_uid)             ptr
    uid              <- (#peek struct fuse_config, uid)                 ptr
    setMode          <- (#peek struct fuse_config, set_mode)            ptr
    umask            <- (#peek struct fuse_config, umask)               ptr
    entryTimeout     <- (#peek struct fuse_config, entry_timeout)       ptr
    negativeTimeout  <- (#peek struct fuse_config, negative_timeout)    ptr
    attrTimeout      <- (#peek struct fuse_config, attr_timeout)        ptr
    intr             <- (#peek struct fuse_config, intr)                ptr
    intrSignal       <- (#peek struct fuse_config, intr_signal)         ptr
    remember         <- (#peek struct fuse_config, remember)            ptr
    hardRemove       <- (#peek struct fuse_config, hard_remove)         ptr
    useIno           <- (#peek struct fuse_config, use_ino)             ptr
    readdirIno       <- (#peek struct fuse_config, readdir_ino)         ptr
    directIo         <- (#peek struct fuse_config, direct_io)           ptr
    kernelCache      <- (#peek struct fuse_config, kernel_cache)        ptr
    autoCache        <- (#peek struct fuse_config, auto_cache)          ptr
    acAttrTimeoutSet <- (#peek struct fuse_config, ac_attr_timeout_set) ptr
    acAttrTimeout    <- (#peek struct fuse_config, ac_attr_timeout)     ptr
    nullpathOk       <- (#peek struct fuse_config, nullpath_ok)         ptr
    pure FuseConfig{..}

  poke ptr FuseConfig{..} = do
    (#poke struct fuse_config, set_gid)             ptr setGid
    (#poke struct fuse_config, gid)                 ptr gid
    (#poke struct fuse_config, set_uid)             ptr setUid
    (#poke struct fuse_config, uid)                 ptr uid
    (#poke struct fuse_config, set_mode)            ptr setMode
    (#poke struct fuse_config, umask)               ptr umask
    (#poke struct fuse_config, entry_timeout)       ptr entryTimeout
    (#poke struct fuse_config, negative_timeout)    ptr negativeTimeout
    (#poke struct fuse_config, attr_timeout)        ptr attrTimeout
    (#poke struct fuse_config, intr)                ptr intr
    (#poke struct fuse_config, intr_signal)         ptr intrSignal
    (#poke struct fuse_config, remember)            ptr remember
    (#poke struct fuse_config, hard_remove)         ptr hardRemove
    (#poke struct fuse_config, use_ino)             ptr useIno
    (#poke struct fuse_config, readdir_ino)         ptr readdirIno
    (#poke struct fuse_config, direct_io)           ptr directIo
    (#poke struct fuse_config, kernel_cache)        ptr kernelCache
    (#poke struct fuse_config, auto_cache)          ptr autoCache
    (#poke struct fuse_config, ac_attr_timeout_set) ptr acAttrTimeoutSet
    (#poke struct fuse_config, ac_attr_timeout)     ptr acAttrTimeout
    (#poke struct fuse_config, nullpath_ok)         ptr nullpathOk

-- | @struct fuse_conn_info@
data FuseConnInfo

-- | @struct fuse_file_info@
data FuseFileInfo

-- | @typedef fuse_fill_dir_t@
type FuseFillDir = Ptr FuseFillDirBuf -> CString -> Ptr FileStat -> COff -> FuseFillDirFlags -> IO CInt

-- | @void@, used in `FuseFillDir`.
data FuseFillDirBuf

-- | @enum fuse_fill_dir_flags@
type FuseFillDirFlags = #type enum fuse_fill_dir_flags

-- | The direct, storable representation of @struct fuse_operations@.
--
-- All operations are optional. NULL indicates undefined operation. You may modify some
-- of the fields to fine-tune the behavior.
--
-- Not to be confused with Haskell-friendly `System.LibFuse3.Internal.FuseOperations`.
-- Also not to be confused with libfuse's low-level API @struct fuse_lowlevel_ops@.
data FuseOperations = FuseOperations
  { fuseGetattr       :: FunPtr CGetattr
  , fuseReadlink      :: FunPtr CReadlink
  , fuseMknod         :: FunPtr CMknod
  , fuseMkdir         :: FunPtr CMkdir
  , fuseUnlink        :: FunPtr CUnlink
  , fuseRmdir         :: FunPtr CRmdir
  , fuseSymlink       :: FunPtr CSymlink
  , fuseRename        :: FunPtr CRename
  , fuseLink          :: FunPtr CLink
  , fuseChmod         :: FunPtr CChmod
  , fuseChown         :: FunPtr CChown
  , fuseTruncate      :: FunPtr CTruncate
  , fuseOpen          :: FunPtr COpen
  , fuseRead          :: FunPtr CRead
  , fuseWrite         :: FunPtr CWrite
  , fuseStatfs        :: FunPtr CStatfs
  , fuseFlush         :: FunPtr CFlush
  , fuseRelease       :: FunPtr CRelease
  , fuseFsync         :: FunPtr CFsync
  , fuseSetxattr      :: FunPtr CSetxattr
  , fuseGetxattr      :: FunPtr CGetxattr
  , fuseListxattr     :: FunPtr CListxattr
  , fuseRemovexattr   :: FunPtr CRemovexattr
  , fuseOpendir       :: FunPtr COpendir
  , fuseReaddir       :: FunPtr CReaddir
  , fuseReleasedir    :: FunPtr CReleasedir
  , fuseFsyncdir      :: FunPtr CFsyncdir
  , fuseInit          :: FunPtr CInit
  , fuseDestroy       :: FunPtr CDestroy
  , fuseAccess        :: FunPtr CAccess
  , fuseCreate        :: FunPtr CCreate
  , fuseLock          :: FunPtr CLock
  , fuseUtimens       :: FunPtr CUtimens
  , fuseBmap          :: FunPtr CBmap
  , fuseIoctl         :: FunPtr CIoctl
  , fusePoll          :: FunPtr CPoll
  , fuseWriteBuf      :: FunPtr CWriteBuf
  , fuseReadBuf       :: FunPtr CReadBuf
  , fuseFlock         :: FunPtr CFlock
  , fuseFallocate     :: FunPtr CFallocate
  , fuseCopyFileRange :: FunPtr CCopyFileRange
  , fuseLseek         :: FunPtr CLseek
  }

instance Storable FuseOperations where
  sizeOf _ = #size struct fuse_operations

  alignment _ = #alignment struct fuse_operations

  peek ptr = do
    fuseGetattr       <- (#peek struct fuse_operations, getattr)         ptr
    fuseReadlink      <- (#peek struct fuse_operations, readlink)        ptr
    fuseMknod         <- (#peek struct fuse_operations, mknod)           ptr
    fuseMkdir         <- (#peek struct fuse_operations, mkdir)           ptr
    fuseUnlink        <- (#peek struct fuse_operations, unlink)          ptr
    fuseRmdir         <- (#peek struct fuse_operations, rmdir)           ptr
    fuseSymlink       <- (#peek struct fuse_operations, symlink)         ptr
    fuseRename        <- (#peek struct fuse_operations, rename)          ptr
    fuseLink          <- (#peek struct fuse_operations, link)            ptr
    fuseChmod         <- (#peek struct fuse_operations, chmod)           ptr
    fuseChown         <- (#peek struct fuse_operations, chown)           ptr
    fuseTruncate      <- (#peek struct fuse_operations, truncate)        ptr
    fuseOpen          <- (#peek struct fuse_operations, open)            ptr
    fuseRead          <- (#peek struct fuse_operations, read)            ptr
    fuseWrite         <- (#peek struct fuse_operations, write)           ptr
    fuseStatfs        <- (#peek struct fuse_operations, statfs)          ptr
    fuseFlush         <- (#peek struct fuse_operations, flush)           ptr
    fuseRelease       <- (#peek struct fuse_operations, release)         ptr
    fuseFsync         <- (#peek struct fuse_operations, fsync)           ptr
    fuseSetxattr      <- (#peek struct fuse_operations, setxattr)        ptr
    fuseGetxattr      <- (#peek struct fuse_operations, getxattr)        ptr
    fuseListxattr     <- (#peek struct fuse_operations, listxattr)       ptr
    fuseRemovexattr   <- (#peek struct fuse_operations, removexattr)     ptr
    fuseOpendir       <- (#peek struct fuse_operations, opendir)         ptr
    fuseReaddir       <- (#peek struct fuse_operations, readdir)         ptr
    fuseReleasedir    <- (#peek struct fuse_operations, releasedir)      ptr
    fuseFsyncdir      <- (#peek struct fuse_operations, fsyncdir)        ptr
    fuseInit          <- (#peek struct fuse_operations, init)            ptr
    fuseDestroy       <- (#peek struct fuse_operations, destroy)         ptr
    fuseAccess        <- (#peek struct fuse_operations, access)          ptr
    fuseCreate        <- (#peek struct fuse_operations, create)          ptr
    fuseLock          <- (#peek struct fuse_operations, lock)            ptr
    fuseUtimens       <- (#peek struct fuse_operations, utimens)         ptr
    fuseBmap          <- (#peek struct fuse_operations, bmap)            ptr
    fuseIoctl         <- (#peek struct fuse_operations, ioctl)           ptr
    fusePoll          <- (#peek struct fuse_operations, poll)            ptr
    fuseWriteBuf      <- (#peek struct fuse_operations, write_buf)       ptr
    fuseReadBuf       <- (#peek struct fuse_operations, read_buf)        ptr
    fuseFlock         <- (#peek struct fuse_operations, flock)           ptr
    fuseFallocate     <- (#peek struct fuse_operations, fallocate)       ptr
    fuseCopyFileRange <- (#peek struct fuse_operations, copy_file_range) ptr
    fuseLseek         <- (#peek struct fuse_operations, lseek)           ptr
    pure FuseOperations{..}

  poke ptr FuseOperations{..} = do
    (#poke struct fuse_operations, getattr)         ptr fuseGetattr
    (#poke struct fuse_operations, readlink)        ptr fuseReadlink
    (#poke struct fuse_operations, mknod)           ptr fuseMknod
    (#poke struct fuse_operations, mkdir)           ptr fuseMkdir
    (#poke struct fuse_operations, unlink)          ptr fuseUnlink
    (#poke struct fuse_operations, rmdir)           ptr fuseRmdir
    (#poke struct fuse_operations, symlink)         ptr fuseSymlink
    (#poke struct fuse_operations, rename)          ptr fuseRename
    (#poke struct fuse_operations, link)            ptr fuseLink
    (#poke struct fuse_operations, chmod)           ptr fuseChmod
    (#poke struct fuse_operations, chown)           ptr fuseChown
    (#poke struct fuse_operations, truncate)        ptr fuseTruncate
    (#poke struct fuse_operations, open)            ptr fuseOpen
    (#poke struct fuse_operations, read)            ptr fuseRead
    (#poke struct fuse_operations, write)           ptr fuseWrite
    (#poke struct fuse_operations, statfs)          ptr fuseStatfs
    (#poke struct fuse_operations, flush)           ptr fuseFlush
    (#poke struct fuse_operations, release)         ptr fuseRelease
    (#poke struct fuse_operations, fsync)           ptr fuseFsync
    (#poke struct fuse_operations, setxattr)        ptr fuseSetxattr
    (#poke struct fuse_operations, getxattr)        ptr fuseGetxattr
    (#poke struct fuse_operations, listxattr)       ptr fuseListxattr
    (#poke struct fuse_operations, removexattr)     ptr fuseRemovexattr
    (#poke struct fuse_operations, opendir)         ptr fuseOpendir
    (#poke struct fuse_operations, readdir)         ptr fuseReaddir
    (#poke struct fuse_operations, releasedir)      ptr fuseReleasedir
    (#poke struct fuse_operations, fsyncdir)        ptr fuseFsyncdir
    (#poke struct fuse_operations, init)            ptr fuseInit
    (#poke struct fuse_operations, destroy)         ptr fuseDestroy
    (#poke struct fuse_operations, access)          ptr fuseAccess
    (#poke struct fuse_operations, create)          ptr fuseCreate
    (#poke struct fuse_operations, lock)            ptr fuseLock
    (#poke struct fuse_operations, utimens)         ptr fuseUtimens
    (#poke struct fuse_operations, bmap)            ptr fuseBmap
    (#poke struct fuse_operations, ioctl)           ptr fuseIoctl
    (#poke struct fuse_operations, poll)            ptr fusePoll
    (#poke struct fuse_operations, write_buf)       ptr fuseWriteBuf
    (#poke struct fuse_operations, read_buf)        ptr fuseReadBuf
    (#poke struct fuse_operations, flock)           ptr fuseFlock
    (#poke struct fuse_operations, fallocate)       ptr fuseFallocate
    (#poke struct fuse_operations, copy_file_range) ptr fuseCopyFileRange
    (#poke struct fuse_operations, lseek)           ptr fuseLseek

-- | @struct fuse_pollhandle@
data FusePollhandle

-- | @enum fuse_readdir_flags@
type FuseReaddirFlags = #type enum fuse_readdir_flags

-- | @struct fuse_session@
data FuseSession

-- | @struct fuse@
data StructFuse

foreign import ccall safe "fuse_cmdline_help"
  fuse_cmdline_help :: IO ()

foreign import ccall safe "fuse_destroy"
  fuse_destroy :: Ptr StructFuse -> IO ()

foreign import ccall safe "fuse_get_session"
  fuse_get_session :: Ptr StructFuse -> IO (Ptr FuseSession)

foreign import ccall safe "fuse_lib_help"
  fuse_lib_help :: Ptr FuseArgs -> IO ()

foreign import ccall safe "fuse_loop_mt_31"
  fuse_loop_mt_31 :: Ptr StructFuse -> CInt -> IO Int

foreign import ccall safe "fuse_lowlevel_help"
  fuse_lowlevel_help :: IO ()

foreign import ccall safe "fuse_lowlevel_version"
  fuse_lowlevel_version :: IO ()

foreign import ccall safe "fuse_mount"
  fuse_mount :: Ptr StructFuse -> CString -> IO CInt

foreign import ccall safe "fuse_new"
  fuse_new :: Ptr FuseArgs -> Ptr FuseOperations -> CSize -> Ptr a -> IO (Ptr StructFuse)

foreign import ccall safe "fuse_opt_free_args"
  fuse_opt_free_args :: Ptr FuseArgs -> IO ()

foreign import ccall safe "fuse_parse_cmdline"
  fuse_parse_cmdline :: Ptr FuseArgs -> Ptr FuseCmdlineOpts -> IO CInt

foreign import ccall safe "fuse_pkgversion"
  fuse_pkgversion :: IO CString

foreign import ccall safe "fuse_session_exit"
  fuse_session_exit :: Ptr FuseSession -> IO ()

foreign import ccall safe "fuse_unmount"
  fuse_unmount :: Ptr StructFuse -> IO ()

type CGetattr = CString -> Ptr FileStat -> Ptr FuseFileInfo -> IO CInt
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

type CChmod = CString -> CMode -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkChmod :: CChmod -> IO (FunPtr CChmod)

type CChown = CString -> CUid -> CGid -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkChown :: CChown -> IO (FunPtr CChown)

type CTruncate = CString -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkTruncate :: CTruncate -> IO (FunPtr CTruncate)

type COpen = CString -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkOpen :: COpen -> IO (FunPtr COpen)

type CRead = CString -> CString -> CSize -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkRead :: CRead -> IO (FunPtr CRead)

type CWrite = CString -> CString -> CSize -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkWrite :: CWrite -> IO (FunPtr CWrite)

type CStatfs = CString -> Ptr FileSystemStats -> IO CInt
foreign import ccall "wrapper"
  mkStatfs :: CStatfs -> IO (FunPtr CStatfs)

type CFlush = CString -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFlush :: CFlush -> IO (FunPtr CFlush)

type CRelease = CString -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkRelease :: CRelease -> IO (FunPtr CRelease)

type CFsync = CString -> CInt -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFsync :: CFsync -> IO (FunPtr CFsync)

type CSetxattr = CString -> CString -> CString -> CSize -> CInt -> IO CInt
foreign import ccall "wrapper"
  mkSetxattr :: CSetxattr -> IO (FunPtr CSetxattr)

type CGetxattr = CString -> CString -> CString -> CSize -> IO CInt
foreign import ccall "wrapper"
  mkGetxattr :: CGetxattr -> IO (FunPtr CGetxattr)

type CListxattr = CString -> CString -> CSize -> IO CInt
foreign import ccall "wrapper"
  mkListxattr :: CListxattr -> IO (FunPtr CListxattr)

type CRemovexattr = CString -> CString -> IO CInt
foreign import ccall "wrapper"
  mkRemovexattr :: CRemovexattr -> IO (FunPtr CRemovexattr)

type COpendir = CString -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkOpendir :: COpendir -> IO (FunPtr COpendir)

type CReaddir = CString -> Ptr FuseFillDirBuf -> FunPtr FuseFillDir -> COff -> Ptr FuseFileInfo -> FuseReaddirFlags -> IO CInt
foreign import ccall "wrapper"
  mkReaddir :: CReaddir -> IO (FunPtr CReaddir)

type CReleasedir = CString -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkReleasedir :: CReleasedir -> IO (FunPtr CReleasedir)

type CFsyncdir = CString -> CInt -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFsyncdir :: CFsyncdir -> IO (FunPtr CFsyncdir)

type CInit = Ptr FuseConnInfo -> Ptr FuseConfig -> IO (Ptr ())
foreign import ccall "wrapper"
  mkInit :: CInit -> IO (FunPtr CInit)

type CDestroy = Ptr () -> IO ()
foreign import ccall "wrapper"
  mkDestroy :: CDestroy -> IO (FunPtr CDestroy)

type CAccess = CString -> CInt -> IO CInt
foreign import ccall "wrapper"
  mkAccess :: CAccess -> IO (FunPtr CAccess)

type CCreate = CString -> CMode -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkCreate :: CCreate -> IO (FunPtr CCreate)

type CLock = CString -> Ptr FuseFileInfo -> CInt -> Ptr Posix.CFLock
foreign import ccall "wrapper"
  mkLock :: CLock -> IO (FunPtr CLock)

-- actual signature:
-- int(*utimens)(const char *, const struct timespec tv[2], struct fuse_file_info *fi)
-- We treat @struct timespec [2]@ as if it is @struct timespec *@ because they are compatible in
-- function parameter lists:
-- https://en.cppreference.com/w/c/language/type#Compatible_types
type CUtimens = CString -> Ptr TimeSpec -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkUtimens :: CUtimens -> IO (FunPtr CUtimens)

type CBmap = CString -> CSize -> Ptr Word64 -> IO CInt
foreign import ccall "wrapper"
  mkBmap :: CBmap -> IO (FunPtr CBmap)

type CIoctl = CString -> CUInt -> Ptr () -> Ptr FuseFileInfo -> CUInt -> Ptr () -> IO CInt
foreign import ccall "wrapper"
  mkIoctl :: CIoctl -> IO (FunPtr CIoctl)

type CPoll = CString -> Ptr FuseFileInfo -> Ptr FusePollhandle -> Ptr CUInt -> IO CInt
foreign import ccall "wrapper"
  mkPoll :: CPoll -> IO (FunPtr CPoll)

type CWriteBuf = CString -> Ptr FuseBufvec -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkWriteBuf :: CWriteBuf -> IO (FunPtr CWriteBuf)

type CReadBuf = CString -> Ptr (Ptr FuseBufvec) -> CSize -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkReadBuf :: CReadBuf -> IO (FunPtr CReadBuf)

type CFlock = CString -> Ptr FuseFileInfo -> CInt -> IO CInt
foreign import ccall "wrapper"
  mkFlock :: CFlock -> IO (FunPtr CFlock)

type CFallocate = CString -> CInt -> COff -> COff -> Ptr FuseFileInfo -> IO CInt
foreign import ccall "wrapper"
  mkFallocate :: CFallocate -> IO (FunPtr CFallocate)

type CCopyFileRange = CString -> Ptr FuseFileInfo -> COff -> CString -> Ptr FuseFileInfo -> COff -> CSize -> CInt -> IO CSsize
foreign import ccall "wrapper"
  mkCopyFileRange :: CCopyFileRange -> IO (FunPtr CCopyFileRange)

type CLseek = CString -> COff -> CInt -> Ptr FuseFileInfo -> IO COff
foreign import ccall "wrapper"
  mkLseek :: CLseek -> IO (FunPtr CLseek)
