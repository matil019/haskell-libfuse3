{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Foreign.C (Errno(Errno), eIO)
import System.LibFuse3
import Test.Hspec (describe, hspec, it, shouldReturn)

deriving instance Show Errno

main :: IO ()
main = hspec $ do
  describe "throwErrnoOf" $ do
    it "should be an inverse of `tryErrno`" $
      tryErrno (throwErrnoOf "" eIO) `shouldReturn` (Left eIO :: Either Errno ())

    it "should be an inverse of `tryErrno_`" $
      tryErrno_ (throwErrnoOf "" eIO) `shouldReturn` eIO
