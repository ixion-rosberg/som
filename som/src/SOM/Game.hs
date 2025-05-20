module SOM.Game (Game (..), game) where

import SOM.Controller (Controller)
import SOM.Map (Map)
import SOM.Physics (Position)
import SOM.Player (Player, player)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

data Game = Game { player ∷ Player, map ∷ Map }

game ∷ Map → Position → SF Controller Game
game m p₀ = proc c → do

  p ← player p₀ ⤙ c

  returnA ⤙ Game p m
