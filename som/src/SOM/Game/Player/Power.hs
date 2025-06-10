module SOM.Game.Player.Power (pmax, power) where

import SOM.Prelude

import SOM.Game.Player.Movement (Movement (..), Speed (..))

import Control.Arrow (arr, returnA, (&&&))

import FRP.Yampa (SF, edge, edgeTag, drSwitch, switch)
import FRP.Yampa.Extra (clampedIntegral)

type Power = Float

pmax ∷ Float
pmax = 100

power ∷ SF Movement Power
power = proc m → do
  rec
    p ← drSwitch regular ⤙ (change m, ev)
    ev ← edgeTag depleted ⤙ p ≡ 0

  returnA ⤙ p

  where
    change = \ case
      Standing        →  40
      Moving Normal _ →  30
      Moving Dash   _ → -60

    regular   = (pmax +) ^≪ clampedIntegral (-pmax, 0)
    depleted  = switch (arr (const 0) &&& recovered) (const (clampedIntegral (0, pmax)))
    recovered = edge ⋘ (≡ (pmax ÷ 2)) ^≪ clampedIntegral (0, pmax ÷ 2)
