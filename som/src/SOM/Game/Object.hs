module SOM.Game.Object (Input (..), Object (..), chest) where

import SOM.Prelude

import SOM.Binary.Animated (Animation, Bounds (..))
import SOM.Game.Animation (Skin (..), animate)
import SOM.Game.Collision (BoundingBox (..), (╳))
import SOM.Game.Player (Player (..))
import SOM.Game.Player.Head (Head (..))
import SOM.Renderer.Draw (Draw)

import Control.Arrow (arr, returnA, (&&&))

import FRP.Yampa (SF, gate, switch)

import Linear.Matrix (M44, identity, mkTransformationMat)
import Linear.Metric (distance)
import Linear.V3 (V3)

data Input = Input { player ∷ Player }

data Object = Object { draw ∷ Draw }

chest ∷ Bounds → Skin → Animation → (M44 Float → Skin → Draw) → V3 Float → SF Input Object
chest b s a d p = proc i → do

  let ev = gate i.player.interact (interactable i.player)

  s' ← switch idle (const (animate a s)) ⤙ ev

  returnA ⤙ Object (d t s')
  where t = mkTransformationMat identity p
        idle = arr (const s) &&& arr id
        interactable pl = abs (distance pl.position p) < 2 ∧ (pl.head.lineOfSight ╳ bb)
        bb = BoundingBox (p + b.min) (p + b.max)
