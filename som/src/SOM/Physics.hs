module SOM.Physics
  ( Acceleration
  , Direction
  , Displacement
  , Position
  , Velocity
  , forward
  , up
  ) where

import SOM.Prelude

import Linear.V3 (V3 (..))

type Acceleration = V3 Float
type Velocity = V3 Float
type Position = V3 Float
type Displacement = V3 Float
type Direction = V3 Float

forward, up ∷ Direction
forward = V3 0 0 -1
up = V3 0 1 0
