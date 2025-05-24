module SOM.Game (Game (..), game) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Map (Map)
import SOM.Player (Player, player)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.V3 (V3)

data Game = Game { player ∷ Player, map ∷ Map }

game ∷ Map → V3 Float → SF Controller Game
game m p₀ = proc c → do

  p ← player m p₀ ⤙ c

  returnA ⤙ Game p m
