{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Foreign.C (CInt, Errno, eIO, eNOENT, eOK)
import System.Clock (Clock(Realtime), getTime)
import System.Fuse3 (FileStat(FileStat), defaultFuseOps, fuseMain)
import System.IO (hPrint, stderr)
import System.Posix.Files (groupReadMode, otherReadMode, ownerReadMode, ownerWriteMode, regularFileMode, unionFileModes)
import System.Posix.User (getRealGroupID, getRealUserID)
import System.Posix.Types (ByteCount, FileOffset)

import qualified Data.ByteString as B
import qualified System.Fuse3 as Fuse

nullGetattr :: FilePath -> IO (Either Errno FileStat)
nullGetattr "/" = do
  fileOwner <- getRealUserID
  fileGroup <- getRealGroupID
  now <- getTime Realtime
  pure $ Right $ FileStat
    { fileID = 0
    , fileMode = foldl' unionFileModes regularFileMode [ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
    , linkCount = 1
    , fileOwner
    , fileGroup
    , specialDeviceID = 0
    , fileSize = 2 ^ (32 :: Int) -- 4G
    , blockCount = 0
    , accessTimeHiRes = now
    , modificationTimeHiRes = now
    , statusChangeTimeHiRes = now
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
  ops = defaultFuseOps
    { Fuse.fuseGetattr = Just $ \fp _ -> nullGetattr fp
    , Fuse.fuseTruncate = Just $ \fp _ _ -> nullTruncate fp
    , Fuse.fuseOpen = Just $ \fp _ _ -> nullOpen fp
    , Fuse.fuseRead = Just $ \fp _ sz off -> nullRead fp sz off
    , Fuse.fuseWrite = Just $ \fp _ buf _ -> nullWrite fp buf
    }
