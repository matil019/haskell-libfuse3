{-# LANGUAGE CApiFFI #-}
module XAttr where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Foreign (Ptr, allocaBytes, castPtr, nullPtr)
import Foreign.C (CInt(CInt), CSize(CSize), CString, peekCStringLen, throwErrnoIfMinus1, throwErrnoIfMinus1_, withCString)
import System.LibFuse3 (SetxattrFlag(SetxattrCreate, SetxattrDefault, SetxattrReplace))
import System.Posix.Types (CSsize(CSsize))

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU

foreign import ccall "lgetxattr"
  c_lgetxattr :: CString -> CString -> Ptr () -> CSize -> IO CSsize

foreign import ccall "llistxattr"
  c_llistxattr :: CString -> CString -> CSize -> IO CSsize

foreign import ccall "lremovexattr"
  c_lremovexattr :: CString -> CString -> IO CInt

foreign import ccall "lsetxattr"
  c_lsetxattr :: CString -> CString -> Ptr () -> CSize -> CInt -> IO CInt

foreign import capi "sys/xattr.h value XATTR_CREATE"
  c_XATTR_CREATE :: CInt

foreign import capi "sys/xattr.h value XATTR_REPLACE"
  c_XATTR_REPLACE :: CInt

get :: FilePath -> String -> IO ByteString
get path name =
  withCString path $ \cpath ->
  withCString name $ \cname -> do
    len <- throwErrnoIfMinus1 "lgetxattr" $ c_lgetxattr cpath cname nullPtr 0
    allocaBytes (fromIntegral len) $ \cvalue -> do
      len2 <- throwErrnoIfMinus1 "lgetxattr" $ c_lgetxattr cpath cname cvalue (fromIntegral len)
      -- fail-fast for the sake of simplicity; serious code should retry C calls
      when (len /= len2) $
        fail "The size of the attribute value changed between lgetxattr calls"
      B.packCStringLen (castPtr cvalue, fromIntegral len2)

list :: FilePath -> IO [String]
list path =
  withCString path $ \cpath -> do
    len <- throwErrnoIfMinus1 "llistxattr" $ c_llistxattr cpath nullPtr 0
    allocaBytes (fromIntegral len) $ \cvalue -> do
      len2 <- throwErrnoIfMinus1 "llistxattr" $ c_llistxattr cpath cvalue (fromIntegral len)
      -- fail-fast for the sake of simplicity; serious code should retry C calls
      when (len /= len2) $
        fail "The size of the attribute value changed between llistxattr calls"
      bs <- BU.unsafePackCStringLen (castPtr cvalue, fromIntegral len2)
      -- use peekCStringLen to make sure we use the same encoding as withCString
      traverse (\b -> BU.unsafeUseAsCStringLen b peekCStringLen) $ B.split 0 bs

remove :: FilePath -> String -> IO ()
remove path name =
  withCString path $ \cpath ->
  withCString name $ \cname ->
  throwErrnoIfMinus1_ "lremovexattr" $ c_lremovexattr cpath cname

set :: FilePath -> String -> ByteString -> SetxattrFlag -> IO ()
set path name value flags =
  withCString path $ \cpath ->
  withCString name $ \cname ->
  BU.unsafeUseAsCStringLen value $ \(cvalue, csize) ->
  throwErrnoIfMinus1_ "lsetxattr" $ c_lsetxattr cpath cname (castPtr cvalue) (fromIntegral csize) (toCFlags flags)
  where
  toCFlags SetxattrDefault = 0
  toCFlags SetxattrCreate = c_XATTR_CREATE
  toCFlags SetxattrReplace = c_XATTR_REPLACE
