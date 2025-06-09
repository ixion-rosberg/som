module SOM.Game (Game (..), game) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Gauge (Gauge)
import SOM.IdentityList (IdentityList)
import SOM.Map (Map)
import SOM.Object (Object)
import SOM.Object qualified as Object (Input (..))
import SOM.Objects (objects)
import SOM.Player (Player (..), player)
import SOM.Player.Power (pmax)
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
