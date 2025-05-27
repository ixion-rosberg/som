module SOM.Object (Object (..), chest) where

import SOM.Prelude

import SOM.Renderer.Draw (Draw)

import Control.Arrow (returnA)

import FRP.Yampa (SF)

import Linear.Matrix (M44, identity, mkTransformationMat)
import Linear.V3 (V3)

data Object = Object { draw ∷ Draw }

chest ∷ (M44 Float → Draw) → V3 Float → SF α Object
chest d p = proc _ → do
  returnA ⤙ Object (d t)
  where t = mkTransformationMat identity p
