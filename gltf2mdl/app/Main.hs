module Main where

import SOM.Prelude

import SOM.Binary.Animated
  ( Animation (..)
  , Bounds (..)
  , Joint (..)
  , Keyframe (..)
  , Model (..)
  , Vertex (..)
  )
import SOM.CLI (Options (..), handlers, outputOrExtension, parser)
import SOM.GlTF
  ( Accessor (..)
  , AnimationSampler (..)
  , AnimationSamplers (..)
  , GlTF (..)
  , Mesh (..)
  , Skin (..)
  , access
  , loadBuffers
  , parse
  )
import SOM.GlTF qualified as G (Animation (..))

import Codec.GlTF (fromFile)

import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Maybe (listToMaybe)
import Data.List (zipWith5)
import Data.List qualified as L (transpose)
import Data.Vector (toList, (!?))

import Linear.Matrix (mkTransformation, (!*!))
import Linear.Matrix qualified as M (transpose)
import Linear.Quaternion (Quaternion (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Linear.Vector (scaled)

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

    animation b g = do
      a ←  maybeToEither "Missing animation" (g.animations !? 0)
      ks ← keyframes b (toList a.samplers)

      pure (Animation ks)

    keyframes b ss = zipWith Keyframe
      <$> (first =≪ mapM (times b) ss)
      <*> (L.transpose <$> mapM (transformations b) ss)

    first = maybeToEither "Missing times" ∘ listToMaybe

    times b s = intervals <$> access b s.translation.input
    intervals = scanl (\ x y → y - x) 0 ∘ fmap (realToFrac ∘ (.unIEEE))

    transformations b s = zipWith3 transformation
      <$> access b s.translation.output
      <*> access b s.rotation.output
      <*> access b s.scale.output

    transformation t r s = mkTransformation r' t' !*! s'
      where
        t' = coerce t
        r' = case coerce r of (V4 x y z w) → Quaternion w (V3 x y z)
        s' = case coerce s of (V3 x y z) → scaled (V4 x y z 1)
