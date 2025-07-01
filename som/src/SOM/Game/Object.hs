module SOM.Game.Object (Input (..), Object (..), ObjectSF, Output (..), Request (..), chest, looseItem) where

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

data Input = Input { player ∷ Player }

data Output = Output { object ∷ Object, request ∷ Event Request }

data Object = Object { position ∷ V3 Float, model ∷ Model }

data Request = Spawn ObjectSF

chest ∷ Bounds → Skin → Animation → (M44 Float → Skin → Model) → V3 Float → Item → ObjectSF
chest b s a d p i = switch closed (const opening)
  where
    closed = arr (const (Output (object s) NoEvent) &&& open)

    opening = output ^≪ (animate a s &&& spawn)
      where spawn          = now (Spawn (looseItem i p))
            output (s', r) = Output (object s') r

    object = Object p ∘ (d (mkTransformationMat identity p))

    open input = gate pl.interact interactable
      where pl           = input.player
            interactable = abs (distance pl.position p) < 2 ∧ (pl.head.lineOfSight ╳ bb)
            bb           = BoundingBox (p + b.min) (p + b.max)

looseItem ∷ Item → V3 Float → ObjectSF
looseItem i p = (arr ∘ const) (Output (Object p (i.model t)) NoEvent)
  where t = mkTransformationMat identity p
