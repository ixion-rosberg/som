module SOM.Normal (Normal (..), pattern N3) where

import SOM.Prelude

import Data.Binary (Binary)

import Linear.V3 (V3 (..))
import Linear.Vector (Additive)

newtype Normal α = Normal { unNormal ∷ V3 α } deriving (Additive, Foldable, Functor, Binary)

pattern N3 ∷ α → α → α → Normal α
pattern N3 x y z ← (Normal (V3 x y z)) where
  N3 x y z = Normal (V3 x y z)
