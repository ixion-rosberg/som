module SOM.Player.Movement (Movement (..), Speed (..), acceleration, headBobbing, movement) where

import SOM.Prelude

import SOM.Controller (Button (..), Dpad (..), Controller (..))

import Control.Arrow (returnA)
import Control.Monad (guard)

import Data.Fixed (mod')
import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import FRP.Yampa (SF, edge, edgeBy, edgeJust, gate, lMerge, switch, time)

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

headBobbing ∷ SF Movement (V3 Float)
headBobbing = (\ y → V3 0 y 0) ^≪ switch standing continue
  where

    continue (m, φ) = switch next continue
      where next = case m of
              Standing   → standing
              Moving s _ → moving s φ

    standing = (0, ) ^≪ edgeJust ≪^ \ case
      Standing   → Nothing
      m          → Just (m, 0)

    moving s₀ φ = proc m → do
      (x, y, ev) ← steps ⤙ ()

      let stopped = gate ev (stop m)

      changedSpeed ← changeSpeed ⤙ m

      returnA ⤙ (y, (m, x) <$ lMerge stopped changedSpeed)

      where
        stop = \ case
          Standing → True
          _        → False

        changeSpeed = edge ≪^ \ case
          Moving s _ → s ≢ s₀
          _          → False

        steps = proc _ → do
          t ← realToFrac ^≪ time ⤙ ()

          let x = mod' (φ + t ÷ period) 1
              y = -0.005 × sin (2 × π × x)

          ev ← stepFinished ⤙ x

          returnA ⤙ (x, y, ev)

        stepFinished = edgeBy (\ x₀ x₁ → guard (x₁ < x₀) $> ()) 0.0

        period = case s₀ of
          Normal → 0.7
          Dash   → 0.5
