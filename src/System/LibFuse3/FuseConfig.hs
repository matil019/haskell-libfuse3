{-# LANGUAGE RecordWildCards #-}
module System.LibFuse3.FuseConfig where

import Foreign.C (CDouble(CDouble), CInt(CInt), CUInt(CUInt))
import System.Posix.Signals (Signal)
import System.Posix.Types (CGid(CGid), CMode(CMode), CUid(CUid), FileMode, GroupID, UserID)

import qualified System.LibFuse3.Internal.C as C

data FuseConfig = FuseConfig
  { -- | @set_gid@
    setGid :: Bool
  , -- | @gid@
    gid :: GroupID
  , -- | @set_uid@
    setUid :: Bool
  , -- | @uid@
    uid :: UserID
  , -- | @set_mode@
    setMode :: Bool
  , -- | @umask@
    umask :: FileMode
  , -- | @entry_timeout@
    entryTimeout :: Double
  , -- | @negative_timeout@
    negativeTimeout :: Double
  , -- | @attr_timeout@
    attrTimeout :: Double
  , -- | @intr@
    intr :: Bool
  , -- | @intr_signal@
    intrSignal :: Signal
  , -- | @remember@
    remember :: Int
  , -- | @hard_remove@
    hardRemove :: Bool
  , -- | @use_ino@
    useIno :: Bool
  , -- | @readdir_ino@
    readdirIno :: Bool
  , -- | @direct_io@
    directIo :: Bool
  , -- | @kernel_cache@
    kernelCache :: Bool
  , -- | @auto_cache@
    autoCache :: Bool
  , -- | @ac_attr_timeout_set@
    acAttrTimeoutSet :: Bool
  , -- | @ac_attr_timeout@
    acAttrTimeout :: Double
  , -- | @nullpath_ok@
    nullpathOk :: Bool
  }
  deriving (Eq, Show)

toCFuseConfig :: FuseConfig -> C.FuseConfig
toCFuseConfig FuseConfig{..} = C.FuseConfig
  { C.setGid           = boolToCInt setGid
  , C.gid              = CUInt $ (\(CGid x) -> x) gid
  , C.setUid           = boolToCInt setUid
  , C.uid              = CUInt $ (\(CUid x) -> x) uid
  , C.setMode          = boolToCInt setMode
  , C.umask            = CUInt $ (\(CMode x) -> x) umask
  , C.entryTimeout     = CDouble entryTimeout
  , C.negativeTimeout  = CDouble negativeTimeout
  , C.attrTimeout      = CDouble attrTimeout
  , C.intr             = boolToCInt intr
  , C.intrSignal       = intrSignal
  , C.remember         = fromIntegral remember
  , C.hardRemove       = boolToCInt hardRemove
  , C.useIno           = boolToCInt useIno
  , C.readdirIno       = boolToCInt readdirIno
  , C.directIo         = boolToCInt directIo
  , C.kernelCache      = boolToCInt kernelCache
  , C.autoCache        = boolToCInt autoCache
  , C.acAttrTimeoutSet = boolToCInt acAttrTimeoutSet
  , C.acAttrTimeout    = CDouble acAttrTimeout
  , C.nullpathOk       = boolToCInt nullpathOk
  }
  where
  boolToCInt b = if b then 1 else 0

fromCFuseConfig :: C.FuseConfig -> FuseConfig
fromCFuseConfig C.FuseConfig{..} = FuseConfig
  { setGid           = cintToBool setGid
  , gid              = CGid $ unCUInt gid
  , setUid           = cintToBool setUid
  , uid              = CUid $ unCUInt uid
  , setMode          = cintToBool setMode
  , umask            = CMode $ unCUInt umask
  , entryTimeout     = unCDouble entryTimeout
  , negativeTimeout  = unCDouble negativeTimeout
  , attrTimeout      = unCDouble attrTimeout
  , intr             = cintToBool intr
  , intrSignal       = intrSignal
  , remember         = fromIntegral $ unCInt remember
  , hardRemove       = cintToBool hardRemove
  , useIno           = cintToBool useIno
  , readdirIno       = cintToBool readdirIno
  , directIo         = cintToBool directIo
  , kernelCache      = cintToBool kernelCache
  , autoCache        = cintToBool autoCache
  , acAttrTimeoutSet = cintToBool acAttrTimeoutSet
  , acAttrTimeout    = unCDouble acAttrTimeout
  , nullpathOk       = cintToBool nullpathOk
  }
  where
  unCDouble (CDouble x) = x
  unCInt (CInt x) = x
  unCUInt (CUInt x) = x
  cintToBool x = x /= 0
