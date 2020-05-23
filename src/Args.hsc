module Args where

#include <fuse.h>

import Foreign (ForeignPtr, Ptr, mallocForeignPtrBytes)
import Foreign.C (CInt(CInt), CString)

data FuseArgs

sizeofFuseArgs :: Int
sizeofFuseArgs = #size struct fuse_args

foreign import ccall "hsfuse3_fuse_args_init"
  c_fuse_args_init :: CInt -> Ptr CString -> Ptr FuseArgs -> IO ()

fuseArgsInit :: [String] -> IO (ForeignPtr FuseArgs)
fuseArgsInit args = do
  ptr <- mallocForeignPtrBytes sizeofFuseArgs
  _
  -- allocaArray (length args) $ \buf ->
  -- c_fuse_args_init (length args) 
