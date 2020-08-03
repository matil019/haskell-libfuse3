{-# LANGUAGE CPP #-}
-- | C land.
--
-- Exported C called from Haskell
module System.Fuse3.Internal.C where

import Foreign (Ptr)
import Foreign.C (CInt(CInt), CSize(CSize), CString)
import System.Fuse3.FileStat (FileStat)
import System.Posix.Types (COff)

-- TODO check peek/poke on structs

data FuseArgs -- struct fuse_args

data FuseBuf -- struct fuse_buf

data FuseCmdlineOpts -- struct fuse_cmdline_opts

data FuseConfig -- struct fuse_config

data FuseConnInfo -- struct fuse_conn_info

data FuseFileInfo -- struct fuse_file_info

-- typedef fuse_fill_dir_t
type FuseFillDir = Ptr FuseFillDirBuf -> CString -> Ptr FileStat -> COff -> FuseFillDirFlags -> IO CInt

data FuseFillDirBuf -- void

type FuseFillDirFlags = CInt -- enum fuse_fill_dir_flags -- TODO give proper type??

data FuseOperations -- struct fuse_operations

type FuseReaddirFlags = CInt -- enum fuse_readdir_flags -- TODO give proper type??

data FuseSession -- struct fuse_session

data StructFuse -- struct fuse

foreign import ccall safe "fuse_mount"
  fuse_mount :: Ptr StructFuse -> CString -> IO CInt

foreign import ccall safe "fuse_unmount"
  fuse_unmount :: Ptr StructFuse -> IO ()

foreign import ccall safe "fuse_get_session"
  fuse_get_session :: Ptr StructFuse -> IO (Ptr FuseSession)

foreign import ccall safe "fuse_session_exit"
  fuse_session_exit :: Ptr FuseSession -> IO ()

foreign import ccall safe "fuse_parse_cmdline"
  fuse_parse_cmdline :: Ptr FuseArgs -> Ptr FuseCmdlineOpts -> IO CInt

foreign import ccall safe "fuse_new"
  fuse_new :: Ptr FuseArgs -> Ptr FuseOperations -> CSize -> Ptr a -> IO (Ptr StructFuse)

foreign import ccall safe "fuse_destroy"
  fuse_destroy :: Ptr StructFuse -> IO ()

foreign import ccall safe "fuse_opt_free_args"
  fuse_opt_free_args :: Ptr FuseArgs -> IO ()

foreign import ccall safe "fuse_loop_mt_31"
  fuse_loop_mt_31 :: Ptr StructFuse -> CInt -> IO Int
