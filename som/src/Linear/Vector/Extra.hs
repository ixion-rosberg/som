module Linear.Vector.Extra (integral) where

import SOM.Prelude

import FRP.Yampa (SF, VectorSpace)
import FRP.Yampa qualified as Y (VectorSpace (..), integral)

import Linear.Metric (Metric, dot)
import Linear.Vector (Additive, zero, (^+^), (*^))

newtype VS φ α = VS { unVS ∷ φ α }

instance (Eq α, Floating α, Additive φ, Metric φ) ⇒ VectorSpace (VS φ α) α where
  zeroVector = VS zero
  (VS x) ^+^ (VS y) = VS (x ^+^ y)
  a *^ (VS x)       = VS (a *^ x)
  dot (VS x) (VS y) = dot x y

integral ∷ (Eq α, Floating α, Additive φ, Metric φ) ⇒ SF (φ α) (φ α)
integral = (.unVS) ^≪ Y.integral ≪^ VS
