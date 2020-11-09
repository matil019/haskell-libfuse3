-- | Miscellaneous utilities provided for convenience.
--
-- These can be used for general purpose and are not directly related to FUSE.
module System.LibFuse3.Utils
  ( -- * Bitsets
    testBitSet

  , -- * Errno
    unErrno, ioErrorToErrno, throwErrnoOf, tryErrno, tryErrno_, tryErrno', tryErrno_'

  , -- * File I/O
    pread, pwrite, c_pread, c_pwrite

  , -- * Marshalling strings
    pokeCStringLen0

  , -- * TimeSpec
    timeSpecToPOSIXTime
  )
  where

import Control.Exception (SomeException, try, tryJust)
import Data.Bits ((.&.), Bits)
import Data.ByteString (ByteString)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign (Ptr, allocaBytes, copyArray, pokeElemOff)
import Foreign.C (CInt(CInt), CSize(CSize), CStringLen, Errno(Errno), eIO, eOK, errnoToIOError, getErrno, throwErrno, throwErrnoIfMinus1, withCStringLen)
import GHC.IO.Exception (IOException(IOError, ioe_errno))
import System.Clock (TimeSpec)
import System.Posix.Types (ByteCount, COff(COff), CSsize(CSsize), Fd(Fd), FileOffset)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.Clock as TimeSpec

-- | Identical to @extra@'s @try_@
try_ :: IO a -> IO (Either SomeException a)
try_ = try

-- | Unwraps the newtype `Errno`.
unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

-- | Attempts to extract an `Errno` from an t`IOError` assuming it is
-- constructed with `errnoToIOError` (typically via `throwErrno`).
ioErrorToErrno :: IOError -> Maybe Errno
ioErrorToErrno IOError{ioe_errno=Just e} = Just $ Errno e
ioErrorToErrno _ = Nothing

-- | Like `throwErrno` but takes an `Errno` as a parameter instead of reading from `getErrno`.
--
-- This is an inverse of `tryErrno`:
--
-- @
-- tryErrno (throwErrnoOf _ e) ≡ pure (Left e)
-- @
throwErrnoOf
  :: String -- ^ textual description of the error location
  -> Errno
  -> IO a
throwErrnoOf loc errno = ioError (errnoToIOError loc errno Nothing Nothing)
  where
  _dummyToSuppressWarnings = error "dummy" getErrno throwErrno

-- | Catches an exception constructed with `errnoToIOError` and extracts `Errno` from it.
tryErrno :: IO a -> IO (Either Errno a)
tryErrno = tryJust ioErrorToErrno

-- | Like `tryErrno` but discards the result of the original action.
--
-- If no exceptions, returns `eOK`.
tryErrno_ :: IO a -> IO Errno
tryErrno_ = fmap (either id (const eOK)) . tryErrno

-- | Like `tryErrno` but also catches non-Errno errors to return `eIO`.
tryErrno' :: IO a -> IO (Either Errno a)
tryErrno' = fmap (either (const $ Left eIO) id) . try_ . tryErrno

-- | Like `tryErrno_` but also catches non-Errno errors to return `eIO`.
tryErrno_' :: IO a -> IO Errno
tryErrno_' = fmap (either (const eIO) id) . try_ . tryErrno_

-- | Converts a `TimeSpec` to a `POSIXTime`.
--
-- This is the same conversion as the @unix@ package does (as of writing).
timeSpecToPOSIXTime :: TimeSpec -> POSIXTime
timeSpecToPOSIXTime ts = fromRational $ TimeSpec.toNanoSecs ts % 10^(9::Int)

-- | Marshals a Haskell string into a NUL terminated C string in a locale-dependent way.
--
-- Does `withCStringLen` and copies it into the destination buffer.
--
-- The Haskell string should not contain NUL characters.
--
-- If the destination buffer is not long enough to hold the source string, it is truncated
-- and a NUL byte is inserted at the end of the buffer.
pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
  withCStringLen src $ \(pSrc, srcSize) -> do
    -- withCStringLen does *not* append NUL byte at the end
    let bufSize0 = bufSize - 1
    copyArray pBuf pSrc (min bufSize0 srcSize)
    pokeElemOff pBuf (min bufSize0 srcSize) 0

-- | @testBitSet bits mask@ is @True@ iff all bits in @mask@ are set in @bits@.
--
-- @
-- testBitSet bits mask ≡ bits .&. mask == mask
-- @
testBitSet :: Bits a => a -> a -> Bool
testBitSet bits mask = bits .&. mask == mask

-- | Reads from a file descriptor at a given offset.
--
-- Fewer bytes may be read than requested.
-- On error, throws an t`IOError` corresponding to the errno.
pread :: Fd -> ByteCount -> FileOffset -> IO ByteString
pread (Fd fd) size off =
  allocaBytes (fromIntegral size) $ \buf -> do
    readBytes <- throwErrnoIfMinus1 "pread" $ c_pread fd buf size off
    B.packCStringLen (buf, fromIntegral readBytes)

-- | Writes to a file descriptor at a given offset.
--
-- Returns the number of bytes written. Fewer bytes may be written than requested.
-- On error, throws an t`IOError` corresponding to the errno.
pwrite :: Fd -> ByteString -> FileOffset -> IO CSsize
pwrite (Fd fd) bs off =
  BU.unsafeUseAsCStringLen bs $ \(buf, size) ->
    throwErrnoIfMinus1 "pwrite" $ c_pwrite fd buf (fromIntegral size) off

-- | A foreign import of @pread(2)@
foreign import ccall "pread"
  c_pread :: CInt -> Ptr a -> CSize -> COff -> IO CSsize

-- | A foreign import of @pwrite(2)@
foreign import ccall "pwrite"
  c_pwrite :: CInt -> Ptr a -> CSize -> COff -> IO CSsize
