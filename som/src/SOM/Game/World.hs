module SOM.Game.World (Output (..), World (..), world) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Game.Map (Map)
import SOM.Game.Object (Object, ObjectSF)
import SOM.Game.Object qualified as Object (Input (..))
import SOM.Game.Objects (objects)
import SOM.Game.Objects qualified as Objects (Inspect, Output (..))
import SOM.Game.Player (Player (..), player)
import SOM.IdentityList (IdentityList)

import Control.Arrow (returnA)

import FRP.Yampa (Event (..), SF)

import Linear.V3 (V3)

data Output = Output { world ∷ World, inspect ∷ Event Objects.Inspect }

data World = World { player ∷ Player, map ∷ Map, objects ∷ IdentityList Object }

world ∷ Map → [ObjectSF] → V3 Float → SF Controller Output
world m xs p₀ = proc c → do

  p ← player m p₀ ⤙ c
  o ← objects xs ⤙ Object.Input p

  returnA ⤙ Output (World p m o.objects) o.inspect
