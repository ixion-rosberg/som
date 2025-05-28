module SOM.Animation (Skin (..), animate) where

import SOM.Prelude

import SOM.Binary.Animated (Animation (..), Keyframe (..), Transformation (..))

import Control.Arrow ((&&&))

import Data.List.NonEmpty (NonEmpty (..))

import FRP.Yampa (SF, Time, after, arr, parB, switch, time)

import Linear.Matrix (M44, mkTransformation, (!*!))
import Linear.Quaternion (slerp)
import Linear.Vector (lerp, scaled)
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

data Skin = Skin { transformations     ∷ [M44 Float]
                 , inverseBindMatrices ∷ [M44 Float]
                 }

animate ∷ Animation → Skin → SF α Skin
animate a s = apply ^≪ parB (animateJoint <$> a.transformations)
  where
    apply ts = Skin ts s.inverseBindMatrices

    animateJoint tr = transformation
      <$> animateV tr.translation
      <*> animateQ tr.rotation
      <*> animateV tr.scale
      where
        animateV = animateProperty (\ x₁ x₂ t → lerp (realToFrac t) x₁ x₂)
        animateQ = animateProperty (\ x₁ x₂ → slerp x₁ x₂ ∘ realToFrac)
        transformation t r (V3 sx sy sz) = mkTransformation r t !*! scaled (V4 sx sy sz 1)

    animateProperty ∷ (β → β → Time → β) → NonEmpty (Keyframe β) → SF α β
    animateProperty f (x₀ :| xs) = animateProperty' x₀.value x₀.time xs
      where
        animateProperty' x d = \ case
          []     → arr (const x)
          k : ks → switch (interpolate f x k.value d) (const (animateProperty' k.value k.time ks))

    interpolate f x₁ x₂ d = (current ^≪ time) &&& after d ()
      where current t = f x₁ x₂ (t ÷ d)
