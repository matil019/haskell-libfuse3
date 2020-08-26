{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Copyright : (The original C)   2001-2007  Miklos Szeredi <miklos@szeredi.hu>
--             (The Haskell port) 2020 yohashi
-- License   : GPL-2
--
-- This \"filesystem\" provides only a single file. The mountpoint
-- needs to be a file rather than a directory. All writes to the
-- file will be discarded, and reading the file always returns \\0.
--
-- This is a port of the C program distributed with the official libfuse.
-- See \"example/null.c\" in the distribution.
module Main where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Foreign.C (CInt, Errno, eIO, eNOENT, eOK)
import System.Clock (Clock(Realtime), getTime)
import System.IO (hPrint, stderr)
import System.LibFuse3 (FileStat, defaultFileStat, defaultFuseOperations, fuseMain)
import System.Posix.Files (groupReadMode, otherReadMode, ownerReadMode, ownerWriteMode, regularFileMode, unionFileModes)
import System.Posix.Types (ByteCount, FileOffset)
import System.Posix.User (getRealGroupID, getRealUserID)

import qualified Data.ByteString as B
import qualified System.LibFuse3 as Fuse

nullGetattr :: FilePath -> IO (Either Errno FileStat)
nullGetattr "/" = do
  fileOwner <- getRealUserID
  fileGroup <- getRealGroupID
  now <- getTime Realtime
  pure $ Right $ defaultFileStat
    { Fuse.fileMode = foldl' unionFileModes regularFileMode [ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
    , Fuse.linkCount = 1
    , Fuse.fileOwner
    , Fuse.fileGroup
    , Fuse.fileSize = 2 ^ (32 :: Int) -- 4G
    , Fuse.accessTimeHiRes = now
    , Fuse.modificationTimeHiRes = now
    , Fuse.statusChangeTimeHiRes = now
    }
nullGetattr _ = pure $ Left eNOENT

nullTruncate :: FilePath -> IO Errno
nullTruncate "/" = pure eOK
nullTruncate _   = pure eNOENT

nullOpen :: FilePath -> IO (Either Errno ())
nullOpen "/" = pure $ Right ()
nullOpen _   = pure $ Left eNOENT

nullRead :: FilePath -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
nullRead "/" size offset
  | offset >= 2 ^ (32 :: Int) = pure $ Right mempty
  | otherwise = pure $ Right $ B.replicate (fromIntegral size) 0
nullRead _ _ _ = pure $ Left eNOENT

nullWrite :: FilePath -> ByteString -> IO (Either Errno CInt)
nullWrite "/" buf = pure $ Right $ fromIntegral $ B.length buf
nullWrite _   _   = pure $ Left eNOENT

main :: IO ()
main = fuseMain ops (\e -> hPrint stderr (e :: SomeException) >> pure eIO)
  where
  ops = defaultFuseOperations
    { Fuse.fuseGetattr = Just $ \fp _ -> nullGetattr fp
    , Fuse.fuseTruncate = Just $ \fp _ _ -> nullTruncate fp
    , Fuse.fuseOpen = Just $ \fp _ _ -> nullOpen fp
    , Fuse.fuseRead = Just $ \fp _ sz off -> nullRead fp sz off
    , Fuse.fuseWrite = Just $ \fp _ buf _ -> nullWrite fp buf
    }
