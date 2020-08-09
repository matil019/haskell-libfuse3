-- | Utils related to @ResourceT@
module System.Fuse3.Internal.Resource where

-- TODO no all-in imports
import Control.Exception
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal
import Foreign
import Foreign.C
import System.Posix.Process

import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode(ExitSuccess))

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

resCallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resCallocBytes = _

resMallocBytes :: Int -> ResourceT IO (ReleaseKey, Ptr a)
resMallocBytes = _

-- | Allocates a block of memory and marshals a value into it like `new`, associating `free` as a cleanup action.
resNew :: Storable a => a -> ResourceT IO (ReleaseKey, Ptr a)
resNew a = allocate (new a) free

resNewCString :: String -> ResourceT IO (ReleaseKey, CString)
resNewCString s = allocate (newCString s) free

resNewFilePath :: FilePath -> ResourceT IO (ReleaseKey, CString)
resNewFilePath = _

resNewArray :: Storable a => [a] -> ResourceT IO (ReleaseKey, Ptr a)
resNewArray = _
