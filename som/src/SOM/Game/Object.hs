module SOM.Game.Object
  ( Input (..)
  , InspectedItem (..)
  , Object (..)
  , ObjectSF
  , Output (..)
  , Request (..)
  , chest
  , looseItem
  ) where

import SOM.Prelude

import SOM.Binary.Animated (Animation)
import SOM.Binary.Bounds (Bounds (..))
import SOM.Game.Animation (Skin (..), animate)
import SOM.Game.Collision (BoundingBox (..), (╳))
import SOM.Game.Item (Item (..))
import SOM.Game.Model (Model (..))
import SOM.Game.Player (Player (..))
import SOM.Game.Player.Head (Head (..))

import Control.Arrow (arr, (&&&))

import FRP.Yampa (Event (..), SF, gate, now, switch)

import Linear.Matrix (M44, identity, mkTransformationMat)
import Linear.Metric (distance)
import Linear.V3 (V3)

type ObjectSF = SF Input Output

data Input = Input { player ∷ Player, interact ∷ Event () }

data Output = Output { object  ∷ Object
                     , request ∷ Event Request
                     , inspect ∷ Event InspectedItem
                     }

data InspectedItem = InspectedItem { item ∷ Item, position ∷ V3 Float }

data Object = Object { position ∷ V3 Float, model ∷ Model }

data Request = Spawn ObjectSF

chest ∷ Bounds → Skin → Animation → (M44 Float → Skin → Model) → V3 Float → Item → ObjectSF
chest b s a d p i = switch closed (const opening)
  where
    closed = arr (const (Output (object s) NoEvent NoEvent) &&& open)

    opening = output ^≪ (animate a s &&& spawn)
      where spawn          = now (Spawn (looseItem i p))
            output (s', r) = Output (object s') r NoEvent

    object = Object p ∘ (d (mkTransformationMat identity p))

    open input = gate input.interact interactable
      where pl           = input.player
            interactable = abs (distance pl.position p) < 1.5 ∧ (pl.head.lineOfSight ╳ bb)
            bb           = BoundingBox (p + b.min) (p + b.max)

looseItem ∷ Item → V3 Float → ObjectSF
looseItem i p = arr (Output (Object p (i.model t)) NoEvent ∘ inspect)
  where
    t             = mkTransformationMat identity p
    inspect input = gate ev (distance input.player.position p < 1.5)
      where ev = InspectedItem i p <$ input.interact
