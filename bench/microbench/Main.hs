module Main where

import Control.Exception (bracket, onException)
import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.ByteString (ByteString)
import Foreign (allocaBytes, free, mallocBytes)
import System.LibFuse3.Utils
import System.Posix.IO (OpenMode(ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (ByteCount, Fd(Fd), FileOffset)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

preadAlloca :: Fd -> ByteCount -> FileOffset -> IO ByteString
preadAlloca (Fd fd) size off =
  allocaBytes (fromIntegral size) $ \buf -> do
    readBytes <- c_pread fd buf size off
    B.packCStringLen (buf, fromIntegral readBytes)

preadMalloc :: Fd -> ByteCount -> FileOffset -> IO ByteString
preadMalloc (Fd fd) size off = do
  buf <- mallocBytes (fromIntegral size)
  let mkBS = do
        readBytes <- c_pread fd buf size off
        BU.unsafePackMallocCStringLen (buf, fromIntegral readBytes)
  mkBS `onException` free buf

main :: IO ()
main = bracket
  (openFd "/dev/zero" ReadOnly Nothing defaultFileFlags)
  closeFd
  $ \fd -> defaultMain
    [ bgroup "pread"
      [ bench "alloca" $ nfIO (preadAlloca fd size off)
      , bench "malloc" $ nfIO (preadMalloc fd size off)
      ]
    ]
  where
  size = 1048576
  off = 1024
