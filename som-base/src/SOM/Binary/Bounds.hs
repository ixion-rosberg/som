module SOM.Binary.Bounds (Bounds (..)) where

import SOM.Prelude

import Data.Binary (Binary)

import GHC.Generics (Generic)

import Linear.V3 (V3)

data Bounds = Bounds { min ∷ V3 Float
                     , max ∷ V3 Float
                     } deriving Generic

instance Binary Bounds
