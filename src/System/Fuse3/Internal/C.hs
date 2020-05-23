-- | C land.
--
-- Exported C called from Haskell
module System.Fuse3.Internal.C where

import Foreign (Ptr)
import Foreign.C (CInt(CInt), CString)
import System.Posix.Types (Fd(Fd))

-- TODO check the type signatures against libfuse3

data FuseArgs -- struct fuse_args

data FuseChan -- struct fuse_chan

data FuseSession -- struct fuse_session

data StructFuse -- struct fuse

data FuseBuf

foreign import ccall safe "fuse_mount"
  fuse_mount :: CString -> Ptr FuseArgs -> IO (Ptr FuseChan)

foreign import ccall safe "fuse_unmount"
  fuse_unmount :: CString -> Ptr FuseChan -> IO ()

foreign import ccall unsafe "fuse_chan_bufsize"
  fuse_chan_bufsize :: Ptr FuseChan -> IO Word -- TODO CWord?

foreign import ccall unsafe "fuse_chan_fd"
  fuse_chan_fd :: Ptr FuseChan -> IO Fd

foreign import ccall safe "fuse_get_session"
  fuse_get_session :: Ptr StructFuse -> IO (Ptr FuseSession)

foreign import ccall safe "fuse_session_exit"
  fuse_session_exit :: Ptr FuseSession -> IO ()

foreign import ccall safe "fuse_parse_cmdline"
  fuse_parse_cmdline :: Ptr FuseArgs -> Ptr CString -> Ptr Int -> Ptr Int -> IO Int

foreign import ccall unsafe "fuse_session_next_chan"
  fuse_session_next_chan :: Ptr FuseSession -> Ptr FuseChan -> IO (Ptr FuseChan)

foreign import ccall safe "fuse_opt_free_args"
  fuse_opt_free_args :: Ptr FuseArgs -> IO ()

foreign import ccall safe "fuse_loop_mt"
  fuse_loop_mt :: Ptr StructFuse -> IO Int

foreign import ccall unsafe "fuse_session_receive_buf"
  fuse_session_receive_buf :: Ptr FuseSession -> Ptr FuseBuf -> Ptr (Ptr FuseChan) -> IO ()

foreign import ccall safe "fuse_session_receive_buf"
  fuse_session_process_buf :: Ptr FuseSession -> Ptr FuseBuf -> Ptr FuseChan -> IO ()
