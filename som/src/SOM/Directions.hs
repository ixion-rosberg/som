module SOM.Directions (pattern FORWARD, pattern RIGHT, pattern UP) where

import SOM.Prelude

import Linear.V3 (V3 (..))

pattern FORWARD, RIGHT, UP ∷ V3 Float
pattern FORWARD = V3 0 0 -1
pattern RIGHT   = V3 1 0  0
pattern UP      = V3 0 1  0
