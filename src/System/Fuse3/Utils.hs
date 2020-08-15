-- | Miscellaneous utilities provided for convenience.
module System.Fuse3.Utils where

import Control.Exception (tryJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (POSIXTime)
import Foreign.C (Errno(Errno), eOK)
import GHC.IO.Exception (IOException(IOError, ioe_errno))
import System.Clock (TimeSpec)

import qualified System.Clock as TimeSpec

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
