module SOM.Player (Player (..), player) where

import SOM.Prelude

import SOM.Collision (BoundingSphere (..), Collision (..), (╳))
import SOM.Controller (Button (..), Controller (..), Dpad (..))
import SOM.Directions (pattern UP)
import SOM.Map (Map, collisionShapes)
import SOM.Normal (Normal (..))
import SOM.Player.Head (Head, head)
import SOM.Player.Head qualified as Head (Input (..))
import SOM.Player.Movement (acceleration, movement)
import SOM.Player.Power (power)

import Control.Arrow (returnA)

import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import FRP.Yampa (SF, integral)

import Linear.Matrix (mkTransformation)
import Linear.Metric (project)
import Linear.Metric.Unicode ((⋅))
import Linear.Quaternion (axisAngle, rotate)
import Linear.V3 (V3 (..))
import Linear.Vector.Extra qualified as V (integral)

data Player = Player { head ∷ Head, power ∷ Float }

player ∷ Map → V3 Float → SF Controller Player
player ma p₀ = proc c → do
  let mo = movement c

  t ← axisAngle UP ^≪ integral ⤙ turn c

  rec
    v ← V.integral            ⤙ acceleration v mo
    p ← (p₀ +) ^≪ V.integral ⤙ (resolveCollisions p ∘ rotate t) v

  h  ← head  ⤙ Head.Input c mo (mkTransformation t p)
  po ← power ⤙ mo

  returnA ⤙ Player h po

  where
    turn c = getSum (turnLeft <> turnRight)
      where
        turnLeft  = mwhen c.dpad.left.held  (Sum  0.6)
        turnRight = mwhen c.dpad.right.held (Sum -0.6)

    resolveCollisions p v = foldr resolve v ((.normal) <$> cs)
      where resolve n x = if n.unNormal ⋅ x < 0
              then x - project n.unNormal x
              else x

            cs = (b ╳) =≪ collisionShapes ma
            b  = BoundingSphere (p + V3 0 0.9 0) 0.5
