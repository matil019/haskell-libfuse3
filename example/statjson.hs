{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright: (c) 2020 yohashi
-- License  : MIT
--
-- A read-only filesystem that exposes file stats in JSON files.
--
-- This filesystem mirrors the content of a source directory along with JSON files generated
-- next to each file. JSON files have \".stat.json\" at the end of their names.
--
-- For example:
--
-- Source directory:
--
-- > a.jpg
-- > b
-- > b/c.txt
--
-- Mounted directory:
--
-- > rootdir.stat.json  <- struct stat of the source directory
-- > a.jpg
-- > a.jpg.stat.json    <- struct stat of a.jpg
-- > b
-- > b.stat.json        <- struct stat of b
-- > b/c.txt
-- > b/c.txt.stat.json  <- struct stat of b/c.txt
--
-- If the source directory contains files which already have \".stat.json\" at the end of
-- their names, they are shadowed (but their own JSON files are still generated, so you'll
-- see \".stat.json.stat.json\" files).
--
-- This example demonstrates the expressiveness of Haskell. It uses the \"layered\"
-- approach. There are three parts in this program; the part that generates
-- \"rootdir.stat.json\", another for generating \".stat.json\" for each file, and the other
-- that provides a read-only view of the source directory. For each of the parts a set of
-- filesystem operations is defined as a layer. Operations may process and respond to
-- filesystem requests themselves or delegate to the next layer. Finally the layers are
-- combined to a @FuseOperations@.
module Main(main) where

import Data.Aeson ((.=))
import Data.Bits ((.&.), (.|.), complement)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Foreign.C (Errno, eIO, eNOSYS, eOPNOTSUPP)
import Numeric (showHex)
import System.Clock (TimeSpec(TimeSpec))
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.Environment (getArgs, withArgs)
import System.FilePath ((<.>), dropTrailingPathSeparator, stripExtension)
import System.LibFuse3
import System.IO (hPrint, stderr)
import System.Posix.IO (OpenFileFlags, OpenMode(ReadOnly), closeFd, openFd)
import System.Posix.Files (regularFileMode, stdFileMode)
import System.Posix.Types (ByteCount, Fd, FileMode, FileOffset)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified System.Clock

#if MIN_VERSION_unix(2,8,0)
import System.Posix.IO (creat)
#endif

openFdCompat :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
#if MIN_VERSION_unix(2,8,0)
openFdCompat path openMode mfileMode openFileFlags = openFd path openMode (openFileFlags{creat = mfileMode})
#else
openFdCompat = openFd
#endif

-- | An outcome of an individual operation in a layer.
data Outcome a
  -- | The request is forwarded to the next layer.
  = Delegate
  -- | The request is handled by the operation.
  | Respond a
  -- | The request is handled by both the operation and the next layer, and the results are
  -- merged by the function.
  | Merge a (a -> a -> a)

type Action a = IO (Either Errno (Outcome a))

-- | A set of operations, or a layer.
data OpsLayer fh = OpsLayer
  { layerGetAttr  :: FilePath -> Action FileStat
  , layerReaddir  :: FilePath -> Action [(String, Maybe FileStat)]
  -- | `Merge` is treated as same as `Respond`
  , layerOpen     :: FilePath -> OpenMode -> OpenFileFlags -> Action fh
  , layerRead     :: fh -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
  , layerRelease  :: fh -> IO ()
  }

removeWriteModes :: FileStat -> FileStat
removeWriteModes stat = stat { fileMode = go (fileMode stat) }
  where
  go mode = mode .&. complement 0o222

-- | Provides the read-only view of the source directory.
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
        ReadOnly -> tryErrno $ fmap Respond $ openFdCompat (srcDir <> path) mode Nothing flags
        _ -> pure $ Left eOPNOTSUPP
  , layerRead = \fd size offset -> tryErrno $ pread fd size offset
  , layerRelease = closeFd
  }

-- | Presents the \"\/roodir.stat.json\" file whose content is the metadata of the source
-- directory.
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

-- | Encodes a `FileStat` into a JSON data.
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

-- | Presents a \".stat.json\" file for each file in the source directory.
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

-- | Chains two `Action`s into one.
--
-- Runs the first `Action` and then run the second if needed.
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

-- | Runs (possibly a chain of) `Action`(s).
--
-- If it returns `Merge`, it is treated as `Respond`.
runAction :: Action a -> IO (Either Errno a)
runAction = fmap $ \case
  Left e -> Left e
  Right Delegate -> Left eNOSYS
  Right (Respond a) -> Right a
  Right (Merge a _) -> Right a

data TheHandle
  = GenRootStatHandle FileStat
  | GenFileStatHandle FileStat
  | PassthroughHandle Fd

-- | Merges the three layers into one, and converts it to `FuseOperations`.
ops :: FilePath -> FuseOperations TheHandle Void
ops srcDir = defaultFuseOperations
  { -- return the response of the first layer to respond.
    fuseGetattr = Just $ \path _ -> runAction $
      layerGetAttr (generateRootStatJsonOps  srcDir) path `chainActions`
      layerGetAttr (generateFileStatJsonOps srcDir) path `chainActions`
      layerGetAttr (passthroughOps          srcDir) path
  , -- return the response of the first layer to respond.
    fuseReaddir = Just $ \path _ -> runAction $
      layerReaddir (generateRootStatJsonOps  srcDir) path `chainActions`
      layerReaddir (generateFileStatJsonOps srcDir) path `chainActions`
      layerReaddir (passthroughOps          srcDir) path
  , -- try each layers in the order and put the result into the 3-way handle.
    -- here we treat `Merge` as if `Respond`.
    fuseOpen = Just $ \path mode flags ->
      layerOpen (generateRootStatJsonOps srcDir) path mode flags `andThen` \case
        Respond x -> pure $ Right $ GenRootStatHandle x
        Merge x _ -> pure $ Right $ GenRootStatHandle x
        Delegate -> layerOpen (generateFileStatJsonOps srcDir) path mode flags `andThen` \case
          Respond x -> pure $ Right $ GenFileStatHandle x
          Merge x _ -> pure $ Right $ GenFileStatHandle x
          Delegate -> layerOpen (passthroughOps srcDir) path mode flags `andThen` \case
            Respond x -> pure $ Right $ PassthroughHandle x
            Merge x _ -> pure $ Right $ PassthroughHandle x
            Delegate -> pure $ Left eNOSYS
  , -- branch on the 3-way handle created by fuseOpen.
    fuseRead = Just $ \_ -> \case
      GenRootStatHandle stat -> layerRead (generateRootStatJsonOps srcDir) stat
      GenFileStatHandle stat -> layerRead (generateFileStatJsonOps srcDir) stat
      PassthroughHandle fd   -> layerRead (passthroughOps srcDir) fd
  , -- branch on the 3-way handle created by fuseOpen.
    fuseRelease = Just $ \_ -> \case
      GenRootStatHandle stat -> layerRelease (generateRootStatJsonOps srcDir) stat
      GenFileStatHandle stat -> layerRelease (generateFileStatJsonOps srcDir) stat
      PassthroughHandle fd   -> layerRelease (passthroughOps srcDir) fd
  }

-- |
--
-- In addition to the common commandline arguments of libfuse applications, this accepts
-- @--source PATH@ which takes the source directory.
main :: IO ()
main = do
  (srcDir', rest) <- do
    args <- getArgs
    case break (=="--source") args of
      (pre, _source:path:suf) -> pure (path, pre <> suf)
      (_, _source:[]) -> fail "`--source` option expects a path"
      (_, []) -> fail "requires an option `--source PATH`"
  srcDir <- dropTrailingPathSeparator <$> makeAbsolute srcDir'
  withArgs rest $ fuseMain (ops srcDir) (\e -> hPrint stderr (e :: IOError) >> pure eIO)
