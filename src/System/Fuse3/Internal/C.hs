-- | C land.
--
-- Exported C called from Haskell
module System.Fuse3.Internal.C where

import Foreign (Ptr)
import Foreign.C (CInt(CInt), CSize(CSize), CString)
import System.Posix.Types (Fd(Fd))

-- TODO check peek/poke on structs

data FuseArgs -- struct fuse_args

data FuseBuf -- struct fuse_buf

data FuseOperations -- struct fuse_operations

data FuseSession -- struct fuse_session

data StructFuse -- struct fuse

data FuseChan -- struct fuse_chan
  -- TODO remove. this struct is not a part of the public API

foreign import ccall safe "fuse_mount"
  fuse_mount :: Ptr StructFuse -> CString -> IO CInt

foreign import ccall safe "fuse_unmount"
  fuse_unmount :: Ptr StructFuse -> IO ()

foreign import ccall unsafe "fuse_chan_bufsize"
  fuse_chan_bufsize :: Ptr FuseChan -> IO Word -- TODO CWord?
  -- TODO remove. this function does not exist

foreign import ccall unsafe "fuse_chan_fd"
  fuse_chan_fd :: Ptr FuseChan -> IO Fd
  -- TODO remove. this function does not exist

foreign import ccall safe "fuse_get_session"
  fuse_get_session :: Ptr StructFuse -> IO (Ptr FuseSession)

foreign import ccall safe "fuse_session_exit"
  fuse_session_exit :: Ptr FuseSession -> IO ()

foreign import ccall safe "fuse_parse_cmdline"
  fuse_parse_cmdline :: Ptr FuseArgs -> Ptr CString -> Ptr Int -> Ptr Int -> IO Int
  -- TODO fuse_parse_cmdline :: Ptr FuseArgs -> Ptr FuseCmdlineOpts -> IO CInt

foreign import ccall unsafe "fuse_session_next_chan"
  fuse_session_next_chan :: Ptr FuseSession -> Ptr FuseChan -> IO (Ptr FuseChan)
  -- TODO remove. this function does not exist

foreign import ccall safe "fuse_new"
  fuse_new :: Ptr FuseArgs -> Ptr FuseOperations -> CSize -> Ptr a -> IO (Ptr StructFuse)

foreign import ccall safe "fuse_destroy"
  fuse_destroy :: Ptr StructFuse -> IO ()

foreign import ccall safe "fuse_opt_free_args"
  fuse_opt_free_args :: Ptr FuseArgs -> IO ()

foreign import ccall safe "fuse_loop_mt"
  fuse_loop_mt :: Ptr StructFuse -> IO Int
  -- TODO this function does not exist. instead,
  -- fuse_loop_mt_31 :: Ptr StructFuse -> CInt -> IO CInt
  -- the caveats may no longer apply; consider fuse_loop and fuse_session_loop

foreign import ccall unsafe "fuse_session_receive_buf"
  fuse_session_receive_buf :: Ptr FuseSession -> Ptr FuseBuf -> Ptr (Ptr FuseChan) -> IO ()
  -- TODO fuse_session_receive_buf :: Ptr FuseSession -> Ptr FuseBuf -> IO CInt

foreign import ccall safe "fuse_session_receive_buf"
  fuse_session_process_buf :: Ptr FuseSession -> Ptr FuseBuf -> Ptr FuseChan -> IO ()
  -- TODO fuse_session_process_buf :: Ptr FuseSession -> Ptr FuseBuf -> IO ()
