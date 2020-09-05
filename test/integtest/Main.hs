module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally)
import Data.Void (Void)
import Foreign.C (eIO)
import System.Environment (withArgs)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.LibFuse3 (FuseOperations, defaultFuseOperations, fuseMain)
import System.Posix.Process (getProcessStatus, forkProcess)
import System.Process (callProcess)

ops :: FuseOperations Void Void
ops = defaultFuseOperations

main :: IO ()
main = withSystemTempDirectory "libfuse3test" $ \mountPoint -> do
  let unmount = do
        hPutStrLn stderr $ "unmounting: " <> mountPoint
        callProcess "fusermount3" ["-u", mountPoint]

  withArgs [mountPoint] $ do
    hPutStrLn stderr $ "mounting a test filesystem on: " <> mountPoint
    pid <- forkProcess $ fuseMain ops (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
    flip finally unmount $ do
      _ <- getProcessStatus True False pid
      threadDelay $ 10 * 1000 * 1000
