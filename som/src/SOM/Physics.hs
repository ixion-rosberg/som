module SOM.Physics (Acceleration, Displacement, Position, Velocity, displacement, velocity) where

import SOM.Prelude

import FRP.Yampa (SF, VectorSpace, integral)
import qualified FRP.Yampa as VS (VectorSpace (..))

import Linear.Metric (dot)
import Linear.V3 (V3 (..))
import Linear.Vector (zero, (^+^), (*^))

type Acceleration = V3 Float
type Velocity = V3 Float
type Position = V3 Float
type Displacement = V3 Float

newtype VS = VS { unVS ∷ V3 Float }

instance VectorSpace VS Float where
  zeroVector = VS zero
  (VS x) ^+^ (VS y) = VS $ x ^+^ y
  a       *^ (VS x) = VS $ a *^ x
  dot (VS x) (VS y) = dot x y

displacement ∷ SF Velocity Displacement
displacement = integral'

integral' ∷ SF (V3 Float) (V3 Float)
integral' = (.unVS) ^≪ integral ≪^ VS

velocity ∷ SF Acceleration Velocity
velocity = integral'
