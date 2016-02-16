module Data.Fixed.Binary ( module Data.Fixed
                         , BE2(..)
                         , BE4(..)) where

import Data.Fixed
import Data.Typeable

data BE2 deriving Typeable
instance HasResolution BE2 where
  resolution _ = 2^2

data BE4 deriving Typeable
instance HasResolution BE4 where
  resolution _ = 2^4
