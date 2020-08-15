{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLoff where

import Foreign (Bits, FiniteBits, Int64, Storable)

#include <sys/types.h>

newtype CLoff = CLoff (#type loff_t)
  deriving (Bits, Bounded, Enum, Eq, FiniteBits, Integral, Num, Ord, Real, Storable)
  deriving newtype (Read, Show)
