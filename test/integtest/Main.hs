module Main where

import Control.Exception (SomeException, finally)
import Data.Bits ((.|.))
import Foreign.C (eIO, eNOENT)
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.LibFuse3
import System.Posix.Files (directoryMode, regularFileMode)
import System.Posix.Process (getProcessStatus, forkProcess)
import System.Process (callProcess)
import Test.HUnit (assertEqual)

import qualified Data.ByteString as B

data TestCase fh dh = TestCase
  { testCaseOps :: FuseOperations fh dh
  , testCaseProg :: FilePath -> IO ()
  }

runTestCase :: TestCase fh dh -> IO ()
runTestCase TestCase{testCaseOps=ops, testCaseProg=prog} =
  withSystemTempDirectory "libfuse3test" $ \mountPoint -> do
    let unmount = do
          hPutStrLn stderr $ "unmounting : " <> mountPoint
          callProcess "fusermount3" ["-u", mountPoint]
    withArgs [mountPoint] $ do
      hPutStrLn stderr $ "mounting on: " <> mountPoint
      pid <- forkProcess $ fuseMain ops (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
      flip finally unmount $ do
        -- wait for fuseMain to daemonize
        _ <- getProcessStatus True False pid
        prog mountPoint

getattrTest :: TestCase fh dh
getattrTest =
  let stat = defaultFileStat
        { fileMode = directoryMode .|. 0o644 -- TODO 0o755
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
        assertEqual "getattrTest" stat stat''
  in TestCase ops prog

-- | If `fuseOpen` is not defined, make sure that `fuseRead` doesn't throw unless
-- the file handle is evaluated.
fuseReadWithoutOpen :: TestCase fh dh
fuseReadWithoutOpen = TestCase
  { testCaseOps = defaultFuseOperations
    { fuseGetattr = Just $ \path mfh ->
        case mfh of
          Just _ -> pure $ Left eIO -- this should never happen
          Nothing
            | path == "/file" -> pure $ Right $ defaultFileStat
                { fileMode = regularFileMode .|. 0o644
                , fileSize = 1
                }
            | otherwise -> pure $ Left eNOENT
    , fuseRead = Just $ \path _fh len off -> path `seq` len `seq` off `seq` (pure $ Right content)
    }

  , testCaseProg = \mountPoint -> do
      readContent <- B.readFile $ mountPoint </> "file"
      assertEqual "fuseReadWithoutOpen" content readContent
  }
  where
  content = B.singleton 0

main :: IO ()
main = do
  runTestCase getattrTest
  runTestCase fuseReadWithoutOpen
