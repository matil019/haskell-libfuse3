-- Please note that @cabal v2-test@ handles the test output very badly for some reason;
-- the output is duplicated and interleaved.
--
-- Use @cabal v2-run -- integtest@ for more reliable output.
module Main where

import Control.Exception (SomeException, finally)
import Data.Bits ((.|.))
import Foreign.C (eIO, eNOENT)
import System.Directory (listDirectory)
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO (hPrint, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.LibFuse3
import System.Posix.Files (directoryMode, regularFileMode)
import System.Posix.Process (getProcessStatus, forkProcess)
import System.Process (callProcess)
import Test.Hspec (Spec, SpecWith, around, describe, hspec, it, shouldBe)

import qualified Data.ByteString as B

-- | Runs a spec on a FUSE filesystem mounted on a temporary directory.
withFileSystem :: FuseOperations fh dh -> SpecWith FilePath -> Spec
withFileSystem ops = around $ \theSpec ->
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
        theSpec mountPoint

main :: IO ()
main = hspec $ do
  -- A basic test of fuseGetattr; we get what we give
  describe "fuseGetattr" $
    let stat = defaultFileStat
          { fileMode = directoryMode .|. 0o755
          , linkCount = 1
          }
        ops = defaultFuseOperations
          { fuseGetattr = Just $ \path _ -> case path of
              "/" -> pure $ Right stat
              _ -> pure $ Left eNOENT
          }
    in withFileSystem ops $ it "getFileStat reads the stat as is" $ \mountPoint -> do
         -- assumes that `getFileStat` works correctly
         stat' <- getFileStat mountPoint
         -- ignore differences in fileID
         -- TODO test use_ino?
         stat' { fileID = fileID stat } `shouldBe` stat

  -- If `fuseOpen` is not defined, make sure that `fuseRead` doesn't throw unless
  -- the file handle is evaluated.
  describe "fuseRead without fuseOpen" $
    let content = B.singleton 0
        ops = defaultFuseOperations
          { fuseGetattr = Just $ \path mfh ->
              case mfh of
                Just _ -> pure $ Left eIO -- this should never happen
                Nothing
                  | path == "/file" -> pure $ Right $ defaultFileStat
                      { fileMode = regularFileMode .|. 0o644
                      , fileSize = 1
                      }
                  | otherwise -> pure $ Left eNOENT
          , fuseRead = Just $ \path _fh len off ->
              -- check that evaluating the args other than the file handle is harmless
              path `seq` len `seq` off `seq` (pure $ Right content)
          }
    in withFileSystem ops $ it "fileRead reads without a crash" $ \mountPoint -> do
         readContent <- B.readFile $ mountPoint </> "file"
         readContent `shouldBe` content

  -- If `fuseOpendir` is not defined, make sure that `fuseReaddir` doesn't throw unless
  -- the directory handle is evaluated.
  describe "fuseReaddir without fuseOpendir" $
    let ops = defaultFuseOperations
          { fuseGetattr = Just $ \path _ ->
              case () of
                _ | path == "/dir/file" -> pure $ Right $ defaultFileStat
                      { fileMode = regularFileMode .|. 0o644
                      , fileSize = 1
                      }
                  | path == "/dir" -> pure $ Right $ defaultFileStat
                      { fileMode = directoryMode .|. 0o755
                      }
                  | otherwise -> pure $ Left eNOENT
          , fuseReaddir = Just $ \path _dh ->
              path `seq` (pure $ Right $ [(".", Nothing), ("..", Nothing), ("file", Nothing)])
          }
    in withFileSystem ops $ it "fileReaddir reads without a crash" $ \mountPoint -> do
         entries <- listDirectory $ mountPoint </> "dir"
         entries `shouldBe` ["file"]
