module SOM.Game (Game (..), game) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Game.Gauge (Gauge)
import SOM.Game.Map (Map)
import SOM.Game.Object (Object)
import SOM.Game.Object qualified as Object (Input (..))
import SOM.Game.Objects (objects)
import SOM.Game.Player (Player (..), player)
import SOM.Game.Player.Power (pmax)
import SOM.IdentityList (IdentityList)
import SOM.Renderer.Draw (Draw)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.V3 (V3)

data Game = Game { player ∷ Player, map ∷ Map, objects ∷ IdentityList Object, powerGauge ∷ Draw }

game ∷ Gauge → Map → [SF Object.Input Object] → V3 Float → SF Controller Game
game g m xs p₀ = proc c → do

  p ← player m p₀ ⤙ c

  os ← objects xs ⤙ Object.Input p

  returnA ⤙ Game p m os (g (p.power ÷ pmax))
