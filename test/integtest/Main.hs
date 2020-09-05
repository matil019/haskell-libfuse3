module Main where

import Control.Exception (SomeException, finally)
import Data.Bits ((.|.))
import Data.Void (Void)
import Foreign.C (eIO, eNOENT)
import System.Environment (withArgs)
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.LibFuse3
import System.Posix.Files (directoryMode)
import System.Posix.Process (getProcessStatus, forkProcess)
import System.Process (callProcess)

data TestCase fh dh = TestCase
  { testCaseOps :: FuseOperations fh dh
  , testCaseProg :: FilePath -> IO ()
  }

runTestCase :: TestCase fh dh -> IO ()
runTestCase TestCase{testCaseOps=ops, testCaseProg=prog} =
  withSystemTempDirectory "libfuse3test" $ \mountPoint -> do
    let unmount = do
          hPutStrLn stderr $ "unmounting: " <> mountPoint
          callProcess "fusermount3" ["-u", mountPoint]
    withArgs [mountPoint] $ do
      hPutStrLn stderr $ "mounting a test filesystem on: " <> mountPoint
      pid <- forkProcess $ fuseMain ops (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
      flip finally unmount $ do
        -- wait for fuseMain to daemonize
        _ <- getProcessStatus True False pid
        prog mountPoint

getattrTest :: TestCase Void Void
getattrTest =
  let stat = defaultFileStat
        { fileMode = directoryMode .|. 0o644
        , linkCount = 1
        }

      ops = defaultFuseOperations
        { fuseGetattr = Just $ \path _ -> case path of
            "/" -> pure $ Right stat
            _ -> pure $ Left eNOENT
        }

      prog = \mountPoint -> do
        -- assumes that `getFileStat` works correctly
        stat' <- getFileStat mountPoint
        -- ignore differences in fileID
        -- TODO test use_ino?
        let stat'' = stat' { fileID = fileID stat }
        print $ stat == stat''
  in TestCase ops prog

main :: IO ()
main = runTestCase getattrTest
