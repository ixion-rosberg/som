module SOM.Player (Player (..), player) where

import SOM.Prelude

import SOM.Controller (Button (..), Controller (..), Dpad (..))
import SOM.Physics (Position, forward, rightOf, up)
import SOM.Player.Movement (acceleration, headBobbing, movement)

import Control.Arrow (returnA)

import FRP.Yampa (SF, integral)

import Linear.Matrix (M44)
import Linear.Projection (lookAt)
import Linear.Quaternion (axisAngle, rotate)
import Linear.V3 (V3 (..))
import Linear.Vector.Extra qualified as V (integral)

data Player = Player { view ∷ M44 Float }

player ∷ Position → SF Controller Player
player p₀ = proc c → do
  let m = movement c

  t ← axisAngle up ^≪ integral ⤙ turn c

  rec
    θl ← integral ⤙ look θl c

  let l = axisAngle (rightOf (rotate t forward)) θl

  rec
    v ← V.integral ⤙ acceleration v m

  p ← (p₀ +) ^≪ V.integral ⤙ rotate t v
  h ← headBobbing ⤙ m

  returnA ⤙ Player (view p h (l × t))

  where
    view p h r = lookAt (ph + h) (ph + dir) up
      where ph = p + V3 0 1.8 0
            dir = rotate r forward

    turn c = case (c.dpad.left.held, c.dpad.right.held) of
      (True , False) →  0.6
      (False, True ) → -0.6
      _              →  0.0

    look θ c = case (c.l2.held, c.r2.held) of
      (True , False) | θ < θmax →  0.6
      (False, True ) | θ > θmin → -0.6
      _                         →  0.0
      where θmax = π ÷ 4
            θmin = -θmax

