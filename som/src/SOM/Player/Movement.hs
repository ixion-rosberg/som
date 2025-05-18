module SOM.Player.Movement (Movement (..), acceleration, movement) where

import SOM.Prelude

import SOM.Controller (Button (..), Dpad (..), Controller (..))
import SOM.Physics

import Linear.Metric (normalize)
import Linear.Vector ((*^))
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

data Movement = Standing | Moving Speed Direction

data Speed = Normal | Dash

type Direction = V2 Float

movement ∷ Controller → Movement
movement c = case (V2 strafe walk) of
  V2 0 0 → Standing
  d      → Moving speed d
  where
    walk   = direction c.dpad.down c.dpad.up
    strafe = direction c.r1 c.l1
    speed  = if c.circle.held then Dash else Normal

    direction pos neg = case (pos.held, neg.held) of
      (True , False) →  1
      (False, True ) → -1
      _              →  0

acceleration ∷ Velocity → Movement → Acceleration
acceleration current m = 6.0 *^ (desired - current)
  where desired = case m of
          Standing → V3 0.0 0.0 0.0
          Moving s (V2 x z) → vmax s *^ normalize (V3 x 0 z)
        vmax = \ case
          Normal → 1.6
          Dash   → 3.2
