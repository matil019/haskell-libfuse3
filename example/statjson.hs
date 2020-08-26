{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Aeson ((.=))
import Data.Bits ((.&.), (.|.), complement)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Foreign (Ptr, allocaBytes)
import Foreign.C (CInt(CInt), CSize(CSize), Errno, eIO, eNOSYS, eOPNOTSUPP)
import Numeric (showHex)
import System.Clock (TimeSpec(TimeSpec))
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.Environment (getArgs, withArgs)
import System.FilePath ((<.>), dropTrailingPathSeparator, stripExtension)
import System.LibFuse3
import System.IO (hPrint, stderr)
import System.Posix.IO (OpenFileFlags, OpenMode(ReadOnly), closeFd, openFd)
import System.Posix.Types (COff(COff), CSsize(CSsize), ByteCount, Fd(Fd), FileOffset)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.Clock

foreign import ccall "pread"
  c_pread :: CInt -> Ptr a -> CSize -> COff -> IO CSsize

data Outcome a = Delegate | Respond a | Merge a (a -> a -> a)

type Action a = IO (Either Errno (Outcome a))

data OpsLayer fh = OpsLayer
  { layerGetAttr  :: FilePath -> Action FileStat
  , layerReaddir  :: FilePath -> Action [(String, Maybe FileStat)]
  , layerOpen     :: FilePath -> OpenMode -> OpenFileFlags -> Action fh
  , layerRead     :: fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
  , layerRelease  :: fh -> IO ()
  }

removeWriteModes :: FileStat -> FileStat
removeWriteModes stat = stat { fileMode = go (fileMode stat) }
  where
  go mode = mode .&. complement 0o222

passthroughOps :: FilePath -> OpsLayer Fd
passthroughOps srcDir = OpsLayer
  -- note that we can't use (</>) to concatenate to srcDir because paths passed from libfuse start with '/'
  -- if srcDir has a trailing slash it makes double slashes but we ignore it
  { layerGetAttr = \path -> tryErrno . fmap Respond . fmap removeWriteModes . getFileStat . (srcDir <>) $ path
  , layerReaddir = \path -> tryErrno $ fmap Respond $ do
      entries <- listDirectory $ srcDir <> path
      pure $ map (\entry -> (entry, Nothing)) entries
  , layerOpen = \path mode flags ->
      case mode of
        ReadOnly -> tryErrno $ fmap Respond $ openFd (srcDir <> path) mode Nothing flags
        _ -> pure $ Left eOPNOTSUPP
  , layerRead = \(Fd fd) size offset -> tryErrno $
      allocaBytes (fromIntegral size) $ \buf -> do
        readBytes <- c_pread fd buf size offset
        B.packCStringLen (buf, fromIntegral readBytes)
  , layerRelease = closeFd
  }

generateRootStatJsonOps :: FilePath -> OpsLayer FileStat
generateRootStatJsonOps srcDir = OpsLayer
  { layerGetAttr = \path ->
      if path /= "/rootdir.stat.json"
        then pure $ Right Delegate
        else tryErrno $ do
          -- borrow the original file's stat for the generated JSON's one
          srcDirStat <- getFileStat $ srcDir
          pure $ Respond $ srcDirStat
            { fileMode = 0o444 .|. entryTypeToFileMode RegularFile
            , linkCount = 1
            , fileSize = fromIntegral $ BL.length $ generateFileStatJson srcDirStat
            }

  , layerReaddir = \path -> pure $ Right $ if path /= "/"
      then Delegate
      else Merge [("rootdir.stat.json", Nothing)] (<>)

  , layerOpen = \path mode _ -> if path /= "/rootdir.stat.json"
      then pure $ Right Delegate
      else case mode of
        ReadOnly -> tryErrno $ fmap Respond $ getFileStat srcDir
        _ -> pure $ Left eOPNOTSUPP

  , layerRead = \stat size off -> pure $ Right
      $ BL.toStrict
      $ BL.take (fromIntegral size)
      $ BL.drop (fromIntegral off)
      $ generateFileStatJson stat

  , layerRelease = \_ -> pure ()
  }

generateFileStatJson :: FileStat -> BL.ByteString
generateFileStatJson stat = Aeson.encode $ Aeson.object
  [ "st_ino"    .= toInt (fileID stat)
  , "st_mode"   .= ("0x" <> showHex (fileMode stat) "")
  , "st_nlink"  .= toInt (linkCount stat)
  , "st_uid"    .= toInt (fileOwner stat)
  , "st_gid"    .= toInt (fileGroup stat)
  , "st_rdev"   .= toInt (specialDeviceID stat)
  , "st_size"   .= toInt (fileSize stat)
  , "st_blocks" .= toInt (blockCount stat)
  , "st_atim"   .= timeSpecToJson (accessTimeHiRes stat)
  , "st_mtim"   .= timeSpecToJson (modificationTimeHiRes stat)
  , "st_ctim"   .= timeSpecToJson (statusChangeTimeHiRes stat)
  ]
  where
  toInt x = fromIntegral x :: Int
  timeSpecToJson TimeSpec{sec, nsec} = Aeson.object [ "sec" .= sec, "nsec" .= nsec ]

generateFileStatJsonOps :: FilePath -> OpsLayer FileStat
generateFileStatJsonOps srcDir = OpsLayer
  { layerGetAttr = \path ->
      case stripExtension ".stat.json" path of
        Nothing -> pure $ Right Delegate
        Just origFilePath -> tryErrno $ do
          origFileExists <- doesPathExist $ srcDir <> origFilePath
          if not origFileExists
            then pure Delegate
            else do
              -- borrow the original file's stat for the generated JSON's one
              origFileStat <- getFileStat $ srcDir <> origFilePath
              pure $ Respond $ removeWriteModes $ origFileStat
                { fileMode = regularFileMode .|. fileMode origFileStat .&. stdFileMode
                , fileSize = fromIntegral $ BL.length $ generateFileStatJson origFileStat
                }

  , layerReaddir = \path -> tryErrno $ do
      origEntries <- listDirectory $ srcDir <> path
      let statJsonEntries = map (\name -> (name, Nothing)) $ map (<.> ".stat.json") origEntries
      pure $ Merge statJsonEntries (<>)

  , layerOpen = \path mode _ ->
      case mode of
        ReadOnly -> case stripExtension ".stat.json" path of
          Nothing -> pure $ Right Delegate
          Just origFilePath -> tryErrno $ do
            origFileExists <- doesPathExist $ srcDir <> origFilePath
            if not origFileExists
              then pure Delegate
              else do
                origFileStat <- getFileStat $ srcDir <> origFilePath
                pure $ Respond origFileStat
        _ -> pure $ Left eOPNOTSUPP

  , layerRead = \stat size off -> pure $ Right
      $ BL.toStrict
      $ BL.take (fromIntegral size)
      $ BL.drop (fromIntegral off)
      $ generateFileStatJson stat

  , layerRelease = \_ -> pure ()
  }

-- poor man's ExceptT
andThen :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
andThen io f = io >>= \case
  Left e -> pure $ Left e
  Right a -> f a

chainActions :: Action a -> Action a -> Action a
chainActions a1 a2 = a1 `andThen` \case
  Respond a  -> pure $ Right $ Respond a
  Delegate   -> a2
  Merge a fa -> a2 >>= pure . fmap (\case
    Delegate   -> Merge a fa
    Respond b  -> Respond $ fa a b
    Merge b fb -> Merge (fa a b) fb
    )

infixl 2 `chainActions`

runAction :: Action a -> IO (Either Errno a)
runAction = fmap $ \case
  Left e -> Left e
  Right Delegate -> Left eNOSYS
  Right (Respond a) -> Right a
  Right (Merge a _) -> Right a

ops :: FilePath -> FuseOperations (Either (Either FileStat FileStat) Fd) Void
ops srcDir = defaultFuseOperations
  { fuseGetattr = Just $ \path _ -> runAction $
      layerGetAttr (generateRootStatJsonOps  srcDir) path `chainActions`
      layerGetAttr (generateFileStatJsonOps srcDir) path `chainActions`
      layerGetAttr (passthroughOps          srcDir) path
  , fuseReaddir = Just $ \path _ -> runAction $
      layerReaddir (generateRootStatJsonOps  srcDir) path `chainActions`
      layerReaddir (generateFileStatJsonOps srcDir) path `chainActions`
      layerReaddir (passthroughOps          srcDir) path
  , fuseOpen = Just $ \path mode flags ->
      layerOpen (generateRootStatJsonOps srcDir) path mode flags `andThen` \case
        Respond x -> pure $ Right $ Left $ Left x
        Merge x _ -> pure $ Right $ Left $ Left x
        Delegate -> layerOpen (generateFileStatJsonOps srcDir) path mode flags `andThen` \case
          Respond x -> pure $ Right $ Left $ Right x
          Merge x _ -> pure $ Right $ Left $ Right x
          Delegate -> layerOpen (passthroughOps srcDir) path mode flags `andThen` \case
            Respond x -> pure $ Right $ Right x
            Merge x _ -> pure $ Right $ Right x
            Delegate -> pure $ Left eNOSYS
  , fuseRead    = Just $ \_ -> either3 (layerRead    (generateRootStatJsonOps srcDir)) (layerRead    (generateFileStatJsonOps srcDir)) (layerRead    (passthroughOps srcDir))
  , fuseRelease = Just $ \_ -> either3 (layerRelease (generateRootStatJsonOps srcDir)) (layerRelease (generateFileStatJsonOps srcDir)) (layerRelease (passthroughOps srcDir))
  }
  where
  either3 :: (a -> r) -> (b -> r) -> (c -> r) -> Either (Either a b) c -> r
  either3 fa fb fc = either (either fa fb) fc

main :: IO ()
main = do
  (srcDir':rest) <- getArgs
  srcDir <- dropTrailingPathSeparator <$> makeAbsolute srcDir'
  withArgs rest $ fuseMain (ops srcDir) (\e -> hPrint stderr (e :: IOError) >> pure eIO)
