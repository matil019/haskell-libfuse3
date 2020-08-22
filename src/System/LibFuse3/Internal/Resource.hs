-- | Utils related to @ResourceT@
--
-- This is an internal module. It is exposed to allow fine-tuning and workarounds but its API is not stable.
module System.LibFuse3.Internal.Resource where

import Control.Exception (catch, mask, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, getInternalState, runInternalState)
import Control.Monad.Trans.Resource.Internal (stateCleanupChecked)
import Foreign (Ptr, Storable, callocBytes, free, mallocBytes, new, newArray)
import Foreign.C (CString, newCString)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Internals (newFilePath)
import System.Posix.Process (exitImmediately, forkProcess)

-- | Forks a new process and transfers the resources to it.
--
-- The parent process `exitImmediately`.
daemonizeResourceT :: ResourceT IO a -> ResourceT IO b
daemonizeResourceT res = do
  -- We don't use resourceForkWith because we don't want to increase refcounts
  istate <- getInternalState
  liftIO $ do
    _ <- forkProcess $ mask $ \restore -> do
      _ <- restore (runInternalState res istate) `catch` \e -> do
        stateCleanupChecked (Just e) istate
        throwIO e
      stateCleanupChecked Nothing istate
    -- cleanup actions are discarded because the child will run them
    exitImmediately ExitSuccess
    undefined

-- | `callocBytes` with `free` associated as a cleanup action.
resCallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resCallocBytes n = allocate (callocBytes n) free

-- | `mallocBytes` with `free` associated as a cleanup action.
resMallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resMallocBytes n = allocate (mallocBytes n) free

-- | `new` with `free` associated as a cleanup action.
resNew :: Storable a => a -> ResourceT IO (ReleaseKey, Ptr a)
resNew a = allocate (new a) free

-- | `newCString` with `free` associated as a cleanup action.
resNewCString :: String -> ResourceT IO (ReleaseKey, CString)
resNewCString s = allocate (newCString s) free

-- | `newFilePath` with `free` associated as a cleanup action.
resNewFilePath :: FilePath -> ResourceT IO (ReleaseKey, CString)
resNewFilePath path = allocate (newFilePath path) free

-- | `newArray` with `free` associated as a cleanup action.
resNewArray :: Storable a => [a] -> ResourceT IO (ReleaseKey, Ptr a)
resNewArray as = allocate (newArray as) free
