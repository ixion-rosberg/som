module SOM.Player.Head where

import SOM.Prelude

import SOM.Controller (Button (..), Controller (..))
import SOM.Directions (pattern FORWARD, pattern RIGHT, pattern UP)
import SOM.Player.Movement (Movement (..), Speed (..))
import SOM.Transform (transform)

import Control.Arrow (returnA)
import Control.Monad (guard)

import Data.Fixed (mod')
import Data.Monoid (Sum (..))
import Data.Monoid.Extra (mwhen)

import FRP.Yampa (SF, edge, edgeBy, edgeJust, edgeTag, gate, integral, lMerge, rSwitch, switch, time)

import Linear.Matrix (M44, mkTransformation, (!*!))
import Linear.Projection (lookAt)
import Linear.Quaternion (axisAngle)
import Linear.V3 (V3 (..))

data Input = Input { controller ∷ Controller, movement ∷ Movement, playerTransformation ∷ M44 Float }

data Head = Head { view ∷ M44 Float }

head ∷ SF Input Head
head = proc i → do
  o ← orientation ⤙ i.controller
  h ← headBobbing ⤙ i.movement

  let t = transformation i.playerTransformation o

  returnA ⤙ (Head (view t h))

  where
    orientation = proc c → do
      rec
        center ← edgeTag integral ⤙ c.l2.held ∧ c.r2.held
        θ      ← rSwitch integral ⤙ (look θ c, center)

      returnA ⤙ axisAngle RIGHT θ

    look θ c = getSum (lookUp <> lookDown)
      where
        lookUp   = mwhen (c.l2.held ∧ θ <  θmax) (Sum  0.7)
        lookDown = mwhen (c.r2.held ∧ θ > -θmax) (Sum -0.7)
        θmax     = π ÷ 4

    transformation t₀ o = t₀ !*! mkTransformation o (V3 0 1.8 0)

    view t p = lookAt (transform t p) (transform t FORWARD) UP


headBobbing ∷ SF Movement (V3 Float)
headBobbing = (\ y → V3 0 y 0) ^≪ switch standing continue
  where

    continue (m, φ) = switch next continue
      where next = case m of
              Standing   → standing
              Moving s _ → moving s φ

    standing = (0, ) ^≪ edgeJust ≪^ \ case
      Standing  → Nothing
      m         → Just (m, 0)

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
