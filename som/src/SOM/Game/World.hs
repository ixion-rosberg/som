module SOM.Game.World (Input (..), ItemAction (..), Output (..), World (..), world) where

import SOM.Prelude

import SOM.Controller (Controller)
import SOM.Game.Map (Map)
import SOM.Game.Object (InspectedItem (..), Object, ObjectSF)
import SOM.Game.Object qualified as Object (Input (..))
import SOM.Game.Objects (objects)
import SOM.Game.Objects qualified as Objects (Input (..), Output (..))
import SOM.Game.Player (Player (..), player)
import SOM.Game.Player qualified as Player (Input (..), Output (..))
import SOM.IdentityList (IdentityList)

import Control.Arrow (returnA)

import FRP.Yampa (Event (..), SF, mapFilterE)

import Linear.V3 (V3)

data Input = Input { controller ∷ Controller
                   , itemAction ∷ Event ItemAction
                   }

data ItemAction = Return InspectedItem
                | Collect InspectedItem

data Output = Output { world   ∷ World
                     , inspect ∷ Event InspectedItem
                     }

data World = World { player  ∷ Player
                   , map     ∷ Map
                   , objects ∷ IdentityList Object
                   }

world ∷ Map → [ObjectSF] → V3 Float → SF Input Output
world m xs p₀ = proc i → do

  p ← player m p₀ ⤙ playerInput i
  o ← objects xs ⤙ objectsInput i p

  returnA ⤙ Output (World p.player m o.objects) o.inspect

  where
    playerInput i = Player.Input i.controller (collectItem i.itemAction)

    objectsInput i p = Objects.Input (objectInput p) (returnItem i.itemAction)

    objectInput p = Object.Input p.player p.interact

    returnItem = mapFilterE \ case
      Return i → Just i
      _        → Nothing

    collectItem = mapFilterE \ case
      Collect i → Just i.item
      _         → Nothing
