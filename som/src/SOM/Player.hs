module SOM.Player (Player (..), player) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Physics (Position, displacement, forward, up, velocity)
import SOM.Player.Movement (acceleration, headBobbing, movement)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.Matrix (M44)
import Linear.Projection (lookAt)
import Linear.V3 (V3 (..))

data Player = Player { view ∷ M44 Float }

player ∷ Position → SF Controller Player
player p₀ = movement ^≫ proc m → do
  rec
    v ← velocity ⤙ acceleration v m

  p ← (p₀ +) ^≪ displacement ⤙ v
  h ← headBobbing ⤙ m

  returnA ⤙ Player (view p h)

  where
    view p h = lookAt (ph + h) (ph + forward) up
      where ph = p + V3 0 1.8 0
