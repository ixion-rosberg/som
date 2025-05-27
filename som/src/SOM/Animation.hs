module SOM.Animation (Skin (..), animate) where

import SOM.Prelude

import SOM.Binary.Animated (Animation (..), Keyframe (..))

import FRP.Yampa (SF, accumHoldBy, afterEach)

import Linear.Matrix (M44)

import Debug.Trace

data Skin = Skin { transformations ∷ [M44 Float], inverseBindMatrices ∷ [M44 Float] }

animate ∷ Animation → Skin → SF α Skin
animate a s = accumHoldBy (\ (Skin _ is) ts → Skin ts is) s ⋘ afterEach (unpack <$> a.keyframes)
  where unpack k = traceShow k.time $ (k.time, k.transformations)
