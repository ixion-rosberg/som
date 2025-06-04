module SOM.Player (Player (..), player) where

import SOM.Prelude

import SOM.Collision (BoundingSphere (..), Collision (..), (╳))
import SOM.Controller (Button (..), Controller (..), Dpad (..))
import SOM.Map (Map, collisionShapes)
import SOM.Normal (Normal (..))
import SOM.Player.Movement (acceleration, headBobbing, movement)
import SOM.Player.Power (power)

import Control.Arrow (returnA)

import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import FRP.Yampa (SF, edgeTag, integral, rSwitch)

import Linear.Matrix (M44)
import Linear.Metric (project)
import Linear.Metric.Unicode ((⋅))
import Linear.Projection (lookAt)
import Linear.Quaternion (axisAngle, rotate)
import Linear.V3 (V3 (..))
import Linear.V3.Unicode qualified as V3 ((×))
import Linear.Vector.Extra qualified as V (integral)

data Player = Player { view ∷ M44 Float, power ∷ Float }

player ∷ Map → V3 Float → SF Controller Player
player ma p₀ = proc c → do
  let mo = movement c

  t ← axisAngle up ^≪ integral ⤙ turn c

  rec
    center ← edgeTag integral ⤙ c.l2.held ∧ c.r2.held
    θl ← rSwitch integral ⤙ (look θl c, center)

  let l = axisAngle ((rotate t forward) V3.× up) θl

  rec
    v ← V.integral ⤙ acceleration v mo
    p ← (p₀ +) ^≪ V.integral ⤙ (resolveCollisions cs ∘ rotate t) v

    let b  = BoundingSphere (p + V3 0 1.8 0) 0.5
        cs = (b ╳) =≪ collisionShapes ma
 
  h ← headBobbing ⤙ mo
  po ← power ⤙ mo

  returnA ⤙ Player (view p h (l × t)) po

  where
    view p h r = lookAt (ph + h) (ph + dir) up
      where ph  = p + V3 0 1.8 0
            dir = rotate r forward

    turn c = getSum (turnLeft <> turnRight)
      where
        turnLeft  = mwhen c.dpad.left.held  (Sum  0.6)
        turnRight = mwhen c.dpad.right.held (Sum -0.6)

    look θ c = getSum (lookUp <> lookDown)
      where
        lookUp   = mwhen (c.l2.held ∧ θ <  θmax) (Sum  0.7)
        lookDown = mwhen (c.r2.held ∧ θ > -θmax) (Sum -0.7)
        θmax     = π ÷ 4

    resolveCollisions cs v = foldr resolve v ((.normal) <$> cs)
      where resolve n x = if n.unNormal ⋅ x < 0
              then x - project n.unNormal x
              else x

    forward = V3 0 0 -1
    up      = V3 0 1 0
