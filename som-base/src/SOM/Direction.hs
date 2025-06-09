module SOM.Direction
  ( Direction (..)
  , pattern D3
  , pattern FORWARD
  , pattern RIGHT
  , pattern UP
  ) where

import SOM.Prelude

import Data.Binary (Binary)

import Linear.V3 (V3 (..))
import Linear.Vector (Additive)

newtype Direction α = Direction { unDirection ∷ V3 α } deriving (Additive, Foldable, Functor, Binary)

pattern D3 ∷ α → α → α → Direction α
pattern D3 x y z ← (Direction (V3 x y z)) where
  D3 x y z = Direction (V3 x y z)

pattern FORWARD, RIGHT, UP ∷ Direction Float
pattern FORWARD = D3 0 0 -1
pattern RIGHT   = D3 1 0  0
pattern UP      = D3 0 1  0
