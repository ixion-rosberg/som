module SOM.Normal (Normal (..)) where

import SOM.Prelude

import Data.Binary (Binary)

import Linear.V3 (V3)
import Linear.Vector (Additive)

newtype Normal α = Normal { unNormal ∷ V3 α } deriving (Additive, Foldable, Functor, Binary)
