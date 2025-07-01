module SOM.Game (Game (..), game) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Draw (Draw)
import SOM.Game.Gauge (Gauge)
import SOM.Game.Map (Map)
import SOM.Game.Object (Object, ObjectSF)
import SOM.Game.Object qualified as Object (Input (..))
import SOM.Game.Objects (objects)
import SOM.Game.Player (Player (..), player)
import SOM.Game.Player.Power (pmax)
import SOM.IdentityList (IdentityList)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.V3 (V3)

data Game = Game { player ∷ Player, map ∷ Map, objects ∷ IdentityList Object, powerGauge ∷ Draw }

game ∷ Gauge → Map → [ObjectSF] → V3 Float → SF Controller Game
game g m xs p₀ = proc c → do

  p ← player m p₀ ⤙ c

  os ← objects xs ⤙ Object.Input p

  returnA ⤙ Game p m os (g (p.power ÷ pmax))
