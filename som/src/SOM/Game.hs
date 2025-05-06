module SOM.Game (Game (..), game) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Map (Map)

import Control.Arrow (arr)

import FRP.Yampa (SF)

data Game = Game { map ∷ Map }

game ∷ Map → SF Controller Game
game = arr ∘ const ∘ Game
