{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.ByteString (ByteString)
import Data.List (foldl')
import Foreign.C (Errno, eNOENT, eOK)
import FileStat (FileStat(FileStat))
import System.Clock (Clock(Realtime), getTime)
import System.Posix.Files (groupReadMode, otherReadMode, ownerReadMode, ownerWriteMode, regularFileMode, unionFileModes)
import System.Posix.User (getRealGroupID, getRealUserID)
import System.Posix.Types (ByteCount, FileOffset)

import qualified Data.ByteString as B
import qualified FileStat

nullGetattr :: FilePath -> IO (Either Errno FileStat)
nullGetattr "/" = do
  fileOwner <- getRealUserID
  fileGroup <- getRealGroupID
  now <- getTime Realtime
  pure $ Right $ FileStat
    { fileMode = foldl' unionFileModes regularFileMode [ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
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

nullTruncate :: FilePath -> FileOffset -> IO Errno
nullTruncate "/" _ = pure eOK
nullTruncate _   _ = pure eNOENT

nullOpen :: FilePath -> IO Errno
nullOpen "/" = pure eOK
nullOpen _   = pure eNOENT

nullRead :: FilePath -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
nullRead "/" size offset
  | offset >= 2 ^ (32 :: Int) = pure $ Right mempty
  | otherwise = pure $ Right $ B.replicate (fromIntegral size) 0
nullRead _ _ _ = pure $ Left eNOENT

nullWrite :: FilePath -> ByteString -> ByteCount -> FileOffset -> IO (Either Errno Int)
nullWrite "/" _ size _ = pure $ Right $ fromIntegral size
nullWrite _   _ _    _ = pure $ Left eNOENT

main :: IO ()
main = pure ()
