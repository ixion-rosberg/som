module SOM.Game (Game (..), Inspection (..), game) where

import SOM.Prelude

import SOM.Controller (Button (..), Controller (..), noInput)
import SOM.Game.Map (Map)
import SOM.Game.Item (Item (..))
import SOM.Game.Object (InspectedItem (..), ObjectSF)
import SOM.Game.World (ItemAction (..), World (..), world)
import SOM.Game.World qualified as World (Input (..), Output (..))
import SOM.Game.Model (Model)

import Control.Arrow (arr, returnA)

import FRP.Yampa (Event (..), SF, initially, integral, lMerge, kSwitch, (-->))

import Linear.Matrix (M44, mkTransformation)
import Linear.Projection (lookAt)
import Linear.V3 (V3 (..))
import Linear.Quaternion (axisAngle)

data Output = Output { game       ∷ Game
                     , transition ∷ Event Transition
                     }

data Game = MainGame World
          | InspectingItem World Inspection

data Inspection = Inspection { model ∷ Model
                             , view  ∷ M44 Float
                             }

data Transition = InspectItem World InspectedItem
                | Resume World GameSF ItemAction

type GameSF = SF World.Input Output

game ∷ Map → [ObjectSF] → V3 Float → SF Controller Game
game m os p₀ = (.game) ^≪ game' mainGame ≪^ input
  where
    game' s = kSwitch s (arr ((.transition) ∘ snd)) transition

    input c = World.Input c NoEvent

    mainGame = output ^≪ world m os p₀
      where
        output w = Output g i
          where
            g = MainGame w.world
            i = InspectItem w.world <$> w.inspect

    transition sf t = game' case t of
      InspectItem w i → inspectingItem sf w i
      Resume w g a    → initialState w --> g ⋘ inject a
      where
        initialState w = Output (MainGame w) NoEvent
        inject = initially ∘ World.Input noInput ∘ Event

    inspectingItem sf w i = (.controller) ^≫ proc c → do
      θ ← integral ⤙ π ÷ 2

      returnA ⤙ output θ c
      where
        output θ c = Output (item θ) (resume c)

        item θ = InspectingItem w (Inspection (i.item.model t) v)
          where
            t = mkTransformation (axisAngle (V3 0.0 1.0 0.0) θ) (V3 0.0 0.0 0.0)
            v = lookAt (V3 0 1 1) (V3 0 0 0) (V3 0 1 0)

        resume c = Resume w sf <$> (lMerge return collect)
          where
            return  = Return i <$ c.x.pressed
            collect = Collect i <$ c.circle.pressed

