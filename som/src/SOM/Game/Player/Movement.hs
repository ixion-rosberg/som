module SOM.Game.Player.Movement (Movement (..), Speed (..), acceleration, movement) where

import SOM.Prelude

import SOM.Controller (Button (..), Dpad (..), Controller (..))

import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import Linear.Metric (normalize)
import Linear.Vector ((*^))
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

data Movement = Standing | Moving Speed Direction

type Direction = V2 Float
type Acceleration = V3 Float
type Velocity = V3 Float

data Speed = Normal | Dash deriving Eq

movement ∷ Controller → Movement
movement c = case (V2 strafe walk) of
  V2 0 0 → Standing
  d      → Moving speed d
  where
    walk   = direction c.dpad.down c.dpad.up
    strafe = direction c.r1 c.l1
    speed  = if c.circle.held then Dash else Normal

    direction bp bn = getSum (mwhen bp.held (Sum 1) <> mwhen bn.held (Sum -1))

acceleration ∷ Velocity → Movement → Acceleration
acceleration current m = 6.0 *^ (desired - current)
  where desired = case m of
          Standing → V3 0.0 0.0 0.0
          Moving s (V2 x z) → vmax s *^ normalize (V3 x 0 z)
        vmax = \ case
          Normal → 1.6
          Dash   → 3.2

