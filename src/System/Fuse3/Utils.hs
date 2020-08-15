-- | Miscellaneous utilities provided for convenience.
module System.Fuse3.Utils where

import Control.Exception (tryJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign (copyArray, pokeElemOff)
import Foreign.C (CInt, CStringLen, Errno(Errno), eOK, withCStringLen)
import GHC.IO.Exception (IOException(IOError, ioe_errno))
import System.Clock (TimeSpec)

import qualified System.Clock as TimeSpec

-- | Unwraps the newtype `Errno`.
unErrno :: Errno -> CInt
unErrno (Errno errno) = errno

-- | Attempts to extract an `Errno` from an `IOError` assuming it is
-- constructed with `errnoToIOError` (typically via `throwErrno`).
ioErrorToErrno :: IOError -> Maybe Errno
ioErrorToErrno IOError{ioe_errno=Just e} = Just $ Errno e
ioErrorToErrno _ = Nothing

-- | Catches an exception constructed with `errnoToIOError`.
tryErrno :: IO a -> IO (Either Errno a)
tryErrno = tryJust ioErrorToErrno

-- | Catches an exception constructed with `errnoToIOError`, discarding the result of the
-- original action.
--
-- If the original action doesn't throw an error, returns `eOK`.
tryErrno_ :: IO a -> IO Errno
tryErrno_ = fmap (either id (const eOK)) . tryErrno

-- | Converts a `TimeSpec` to a `POSIXTime`.
--
-- This is the same conversion as the @unix@ package does (as of writing).
timeSpecToPOSIXTime :: TimeSpec -> POSIXTime
timeSpecToPOSIXTime ts = fromRational $ TimeSpec.toNanoSecs ts % 10^(9::Int)

-- | Marshals a Haskell string into a NUL terminated C string in a locale-dependent way.
--
-- Does `withCStringLen` and copies it into the destination buffer.
--
-- If the destination buffer is not long enough to hold the source string, it is truncated and a
-- NUL byte is appended at the end of the buffer.
pokeCStringLen0 :: CStringLen -> String -> IO ()
pokeCStringLen0 (pBuf, bufSize) src =
  withCStringLen src $ \(pSrc, srcSize) -> do
    -- withCStringLen does *not* append NUL byte at the end
    let bufSize0 = bufSize - 1
    copyArray pBuf pSrc (min bufSize0 srcSize)
    pokeElemOff pBuf (min bufSize0 srcSize) 0
