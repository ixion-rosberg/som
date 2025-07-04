module SOM.Game.Player (Player (..), player) where

import SOM.Prelude

import SOM.Controller (Button (..), Controller (..), Dpad (..))
import SOM.Direction (Direction (..), pattern UP)
import SOM.Game.Collision (BoundingSphere (..), Collision (..), (╳))
import SOM.Game.Map (Map, collisionShapes)
import SOM.Game.Player.Head (Head, head)
import SOM.Game.Player.Head qualified as Head (Input (..))
import SOM.Game.Player.Movement (acceleration, movement)
import SOM.Game.Player.Power (power)

import Control.Arrow (returnA)

import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import FRP.Yampa (Event, SF, integral)

import Linear.Matrix (mkTransformation)
import Linear.Metric (project)
import Linear.Metric.Unicode ((⋅))
import Linear.Quaternion (axisAngle, rotate)
import Linear.V3 (V3 (..))
import Linear.Vector.Extra qualified as V (integral)

data Player = Player { position ∷ V3 Float, head ∷ Head, power ∷ Float, interact ∷ Event () }

player ∷ Map → V3 Float → SF Controller Player
player ma p₀ = proc c → do
  let mo = movement c

  t ← axisAngle (UP).unDirection ^≪ integral ⤙ turn c

  rec
    v ← V.integral            ⤙ acceleration v mo
    p ← (p₀ +) ^≪ V.integral ⤙ (resolveCollisions p ∘ rotate t) v

  h  ← head  ⤙ Head.Input c mo (mkTransformation t p)
  po ← power ⤙ mo

  returnA ⤙ Player p h po c.circle.pressed

  where
    turn c = getSum (turnLeft <> turnRight)
      where
        turnLeft  = mwhen c.dpad.left.held  (Sum  0.6)
        turnRight = mwhen c.dpad.right.held (Sum -0.6)

    resolveCollisions p v = foldr resolve v ((.normal) <$> cs)
      where resolve n x = if n.unDirection ⋅ x < 0
              then x - project n.unDirection x
              else x

            cs = (b ╳) =≪ collisionShapes ma
            b  = BoundingSphere (p + V3 0 0.9 0) 0.5
