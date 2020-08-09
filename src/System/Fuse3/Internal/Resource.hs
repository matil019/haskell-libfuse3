-- | Utils related to @ResourceT@
module System.Fuse3.Internal.Resource where

-- TODO no all-in imports
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal
import Foreign
import Foreign.C

resCallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resCallocBytes = _

resMallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resMallocBytes = _

-- | Allocates a block of memory and marshals a value into it like `new`, associating `free` as a cleanup action.
resNew :: Storable a => a -> ResourceT IO (ReleaseKey, Ptr a)
resNew a = allocate (new a) free

resNewCString :: String -> ResourceT IO (ReleaseKey, CString)
resNewCString s = allocate (newCString s) free

resNewArray :: Storable a => [a] -> ResourceT IO (ReleaseKey, Ptr a)
resNewArray = _
