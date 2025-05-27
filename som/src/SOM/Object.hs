module SOM.Object (Object (..), chest) where

import SOM.Prelude

import SOM.Animation (Skin (..), animate)
import SOM.Binary.Animated (Animation, Joint (..), Model (..))

import SOM.Renderer.Draw (Draw)

import Control.Arrow (arr, returnA, (&&&))

import FRP.Yampa (Event, SF, switch)

import Linear.Matrix (M44, identity, mkTransformationMat)
import Linear.V3 (V3)


data Object = Object { draw ∷ Draw }

chest ∷ Skin → Animation → (M44 Float → Skin → Draw) → V3 Float → SF (Event ()) Object
chest s a d p = proc ev → do

  s' ← switch idle (const (animate a s)) ⤙ ev

  returnA ⤙ Object (d t s')
  where t = mkTransformationMat identity p
        idle = arr (const s) &&& arr id
