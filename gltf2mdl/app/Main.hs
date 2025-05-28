module Main where

import SOM.Prelude

import SOM.Binary.Animated
  ( Animation (..)
  , Bounds (..)
  , Joint (..)
  , Keyframe (..)
  , Model (..)
  , Transformation (..)
  , Vertex (..)
  )
import SOM.CLI (Options (..), handlers, outputOrExtension, parser)
import SOM.GlTF
  ( Accessor (..)
  , AnimationSampler (..)
  , AnimationSamplers (..)
  , Buffers
  , GlTF (..)
  , Mesh (..)
  , Skin (..)
  , access
  , loadBuffers
  , parse
  )
import SOM.GlTF qualified as G (Animation (..))

import Codec.GlTF (fromFile)

import Data.Binary (Binary, encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.List (zipWith5)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Vector (toList, (!?))

import Linear.Matrix qualified as M (transpose)
import Linear.Quaternion (Quaternion (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

import Options.Applicative (execParser, fullDesc, info)

import UnliftIO.Exception
  ( catches
  , fromEither
  , stringException
  )

main ∷ IO ()
main = (flip catches) handlers do
  o ← execParser (info parser fullDesc)
  g ← (lift ∘ parse ↢ lift ↢ fromFile) o.input
  b ← loadBuffers g

  m ← lift $ model b g

  encodeFile (outputOrExtension "mdl" o) m

  where
    lift = fromEither ∘ mapLeft stringException

    model b g = Model
      <$> vertices b g.mesh
      <*> indices b g.mesh
      <*> joints b g
      <*> pure (bounds g.mesh)
      <*> animation b g

    vertices b m = zipWith5 vertex
      <$> access b m.position.bufferView
      <*> access b m.normal
      <*> access b m.texCoord
      <*> accessMaybe b "Missing joints" m.joints
      <*> accessMaybe b "Missing weights" m.weights

    vertex p n t j w = Vertex (coerce p) (coerce n) (coerce t) (fmap fromIntegral j) (coerce w)

    accessMaybe b m = access b ↢ maybeToEither m

    indices b m = coerce <$> access b m.indices

    joints b g = do
      s ← maybeToEither "Missing skin" g.skin

      zipWith joint (toList s.joints) <$> access b s.inverseBindMatrices

    joint t m = Joint t (coerce ∘ M.transpose $ m)

    bounds m = Bounds (coerce m.position.min) (coerce m.position.max)

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
