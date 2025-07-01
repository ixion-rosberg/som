module SOM.Game (Game (..), InspectedItem (..), game) where

import SOM.Prelude

import SOM.Controller (Button (..), Controller (..))
import SOM.Game.Map (Map)
import SOM.Game.Item (Item (..))
import SOM.Game.Object (ObjectSF)
import SOM.Game.World (World (..), world)
import SOM.Game.World qualified as World (Output (..))
import SOM.Game.Model (Model)
import SOM.IdentityList (delete)

import Control.Arrow (arr, returnA)

import Data.Function ((&))

import FRP.Yampa (Event (..), SF, integral, kSwitch)

import Linear.Matrix (M44, mkTransformation)
import Linear.Projection (lookAt)
import Linear.V3 (V3 (..))
import Linear.Quaternion (axisAngle)

data Output = Output { game ∷ Game, transition ∷ Event Transition }

data Game = MainGame World | InspectingItem World InspectedItem

data InspectedItem = InspectedItem { model ∷ Model, view ∷ M44 Float }

data Transition = InspectItem World ℕ Item | Resume GameSF

type GameSF = SF Controller Output

game ∷ Map → [ObjectSF] → V3 Float → SF Controller Game
game m os p₀ = (.game) ^≪ game' mainGame
  where
    game' s = kSwitch s (arr ((.transition) ∘ snd)) transition

    mainGame = output ^≪ world m os p₀
      where
        output w = Output g i
          where
            g = MainGame w.world
            i = (\ (id, it) → InspectItem w.world id it) <$> w.inspect

    transition sf t = game' case t of
      InspectItem w id it → inspectingItem sf w id it
      Resume g            → g

    inspectingItem sf w id it = proc c → do
      θ ← integral ⤙ π ÷ 2

      returnA ⤙ Output (InspectingItem w' (item θ)) ((Resume sf) <$ c.x.pressed)

      where
        w'     = w & (\ World {..} → World { objects = delete id w.objects, .. })
        item θ = InspectedItem (it.model t) v
          where t = mkTransformation (axisAngle (V3 0.0 1.0 0.0) θ) (V3 0.0 0.0 0.0)
                v = lookAt (V3 0 1 1) (V3 0 0 0) (V3 0 1 0)
