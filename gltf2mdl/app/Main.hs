module Main where

import SOM.Prelude

import SOM.Binary.Animated
  ( Animation (..)
  , Joint (..)
  , Keyframe (..)
  , Model (..)
  , Transformation (..)
  , Vertex (..)
  )
import SOM.Binary.Bounds (Bounds (..))
import SOM.CLI (Options (..), fromEither, outputOrExtension, parseOptions, runCLI)
import SOM.GlTF
  ( Accessor (..)
  , AnimationSampler (..)
  , AnimationSamplers (..)
  , Buffers
  , GlTF (..)
  , Image (..)
  , Mesh (..)
  , Skin (..)
  , access
  , accessMaybe
  , fromFile
  , loadBuffers
  )
import SOM.GlTF qualified as G (Animation (..))

import Data.Binary (Binary, encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Coerce (coerce)
import Data.Either.Extra (maybeToEither)
import Data.List (zipWith5)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Vector (toList, (!?))

import Linear.Matrix qualified as M (transpose)
import Linear.Quaternion (Quaternion (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

main ∷ IO ()
main = runCLI do
  o ← parseOptions

  g ← (fromEither ↢ fromFile) o.input
  b ← loadBuffers g
  m ← fromEither (model b g)

  encodeFile (outputOrExtension "mdl" o) m

  where
    model b g = Model
      <$> vertices  b g.mesh
      <*> indices   b g.mesh
      <*> joints    b g
      <#> bounds      g.mesh.position
      <#> texture     g.image
      <*> animation b g

    vertices b m = zipWith5 vertex
      <$> access                        b m.position.bufferView
      <*> access                        b m.normal
      <*> access                        b m.texCoord
      <*> accessMaybe "Missing joints"  b m.joints
      <*> accessMaybe "Missing weights" b m.weights

    vertex p n t j w = Vertex (coerce p) (coerce n) (coerce t) (fmap fromIntegral j) (coerce w)

    indices b m = coerce <$> access b m.indices

    joints b g = do
      s ← maybeToEither "Missing skin" g.skin

      zipWith joint (toList s.joints) <$> access b s.inverseBindMatrices

    joint t = Joint t ∘ coerce ∘ M.transpose

    bounds p = Bounds (coerce p.min) (coerce p.max)

    texture i = i.name <> ".txr"

    animation b g = Animation
      <$> (transformations b =≪ maybeToEither "Missing animation" (g.animations !? 0))

    transformations b = mapM (transformation b) ∘ toList ∘ (.samplers)

    transformation b ss = Transformation
      <$> keyframes coerce b ss.translation
      <*> keyframes rotation b ss.rotation
      <*> keyframes coerce b ss.scale

    keyframes ∷ Binary α ⇒ (α → β) → Buffers → AnimationSampler α → Either String (NonEmpty (Keyframe β))
    keyframes f b s = do
      ts ← access b s.input
      xs ← access b s.output

      nonEmptyList (zipWith Keyframe (intervals ts) (f <$> xs))

    nonEmptyList = maybeToEither "Missing keyframes" ∘ nonEmpty

    intervals = intervals' 0 ∘ fmap (realToFrac ∘ (.unIEEE))
    intervals' t₀ = \ case
      []     → []
      t : ts → (t - t₀) : (intervals' t ts)

    rotation = (\ (V4 x y z w) → Quaternion w (V3 x y z)) ∘ coerce
