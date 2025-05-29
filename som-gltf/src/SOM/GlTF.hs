module SOM.GlTF
  ( Accessor (..)
  , Animation (..)
  , AnimationSampler (..)
  , AnimationSamplers (..)
  , Buffers
  , BufferView (..)
  , GlTF (..)
  , Image (..)
  , Mesh (..)
  , Skin (..)
  , access
  , loadBuffers
  , parse
  ) where

import SOM.Prelude

import Codec.GlTF qualified as Unparsed (GlTF (..))
import Codec.GlTF.Accessor qualified as Unparsed (Accessor (..), AccessorIx (..))
import Codec.GlTF.Animation
  ( pattern ROTATION
  , pattern SCALE
  , pattern TRANSLATION
  )
import Codec.GlTF.Animation qualified as Unparsed
  ( Animation (..)
  , AnimationChannel (..)
  , AnimationChannelTarget (..)
  , AnimationSampler (..)
  , AnimationSamplerIx (..)
  )
import Codec.GlTF.Buffer qualified as Unparsed (Buffer (..), BufferIx (..))
import Codec.GlTF.BufferView qualified as Unparsed (BufferView (..), BufferViewIx (..))
import Codec.GlTF.Image qualified as Unparsed (Image (..))
import Codec.GlTF.Mesh qualified as Unparsed (Mesh (..), MeshPrimitive (..))
import Codec.GlTF.Node (Node (..), NodeIx (..))
import Codec.GlTF.Node qualified as Unparsed (Skin (..))
import Codec.GlTF.URI (URI, loadURI)

import Data.Binary (Binary)
import Data.Binary.Extra (IEEE (..), UnsignedShort, decodeMany)
import Data.ByteString (ByteString, fromStrict, readFile)
import Data.ByteString qualified as B (drop, take)
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.HashMap.Strict (lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Vector (Vector, find, toList, (!?))
import Data.Vector qualified as V (filter)
import Data.Word (Word8)

import Linear.Matrix (M44, mkTransformation, (!*!))
import Linear.Quaternion (Quaternion (..))
import Linear.V2 (V2)
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))
import Linear.Vector (scaled)

import UnliftIO.Exception (fromEither, stringException)

data GlTF = GlTF { buffers    ∷ Vector URI
                 , mesh       ∷ Mesh
                 , image      ∷ Image
                 , skin       ∷ Maybe Skin
                 , animations ∷ Vector Animation
                 }

data Mesh = Mesh { position ∷ Accessor (V3 IEEE)
                 , normal   ∷ BufferView (V3 IEEE)
                 , texCoord ∷ BufferView (V2 IEEE)
                 , indices  ∷ BufferView UnsignedShort
                 , joints   ∷ Maybe (BufferView (V4 Word8))
                 , weights  ∷ Maybe (BufferView (V4 IEEE))
                 }

data Accessor α = Accessor { bufferView ∷ BufferView α, min ∷ α, max ∷ α }

data BufferView α = BufferView { buffer ∷ Int, length ∷ Int, offset ∷ Int }

newtype Image = Image { name ∷ String }

data Skin = Skin { joints ∷ Vector (M44 Float), inverseBindMatrices ∷ BufferView (M44 IEEE) }

newtype Animation = Animation { samplers ∷ Vector AnimationSamplers }

data AnimationSamplers = AnimationSamplers { translation ∷ AnimationSampler (V3 IEEE)
                                           , rotation    ∷ AnimationSampler (V4 IEEE)
                                           , scale       ∷ AnimationSampler (V3 IEEE)
                                           }

data AnimationSampler α = AnimationSampler { input ∷ BufferView IEEE, output ∷ BufferView α }

type Buffers = Vector ByteString

parse ∷ Unparsed.GlTF → Either String GlTF
parse g = do
  vs ← bufferViews
  as ← accessors
  bs ← buffers
  m ← mesh as vs
  i ← image

  pure (GlTF bs m i (skin as vs) (animations as vs))

  where
    buffers = maybeToEither "Missing buffers" $ mapM (.uri) =≪ g.buffers
    bufferViews = maybeToEither "Missing buffer views" g.bufferViews
    accessors = maybeToEither "Missing accessors" g.accessors
    mesh as vs = (maybeToEither "Missing mesh" ∘ (parseMesh as vs ↢ findMesh)) g
    findMesh = (!? 0) ∘ (.primitives) ↢ (!? 0) ↢ (.meshes)
    parseMesh as vs m = Mesh
      <$> (accessor v3  =≪ attribute "POSITION")
      <*> (bufferView vs =≪ attribute "NORMAL")
      <*> (bufferView vs =≪ attribute "TEXCOORD_0")
      <*> (bufferView vs =≪ indices)
      <*> (pure $ bufferView vs =≪ attribute "JOINTS_0")
      <*> (pure $ bufferView vs =≪ attribute "WEIGHTS_0")
      where
        accessor f a = Accessor <$> bufferView vs a <*> (f =≪ a.min) <*> (f =≪ a.max)

        attribute ∷ Text → Maybe (Unparsed.Accessor)
        attribute a = accessor' (lookup a m.attributes)
        indices = accessor' m.indices
        accessor' i = ((as !?) =≪ fmap ((.unAccessorIx)) i)

        v3 = toList ⋙ \ case
          [x, y, z] → (Just ∘ fmap (IEEE ∘ realToFrac)) (V3 x y z)
          _         → Nothing

    image = (maybeToEither "Missing image" ∘ (parseImage ↢ findImage)) g
    findImage = (!? 0) ↢ (.images)
    parseImage = fmap (Image ∘ unpack) ∘ (.name)

    skin as vs = do
      s ← (!? 0) =≪ g.skins
      ns ← g.nodes

      js ← mapM (fmap joint ∘ (ns !?) ∘ (.unNodeIx)) s.joints
      is ←  (bufferView vs ↢ (as !?) ∘ (.unAccessorIx)) =≪ s.inverseBindMatrices

      pure $ Skin js is

    joint n = transformation
      (fromMaybe (0, 0, 0)    n.translation)
      (fromMaybe (0, 0, 0, 0) n.rotation)
      (fromMaybe (1, 1, 1)    n.scale)
      where
        transformation t r s = mkTransformation (rotation r) (translation t) !*! scale s
        translation (x, y, z) = V3 x y z
        rotation (x, y, z, w) = Quaternion w (V3 x y z)
        scale (x, y, z) = scaled (V4 x y z 1)

    animations as vs = fromMaybe mempty $ mapM (animation as vs) =≪ g.animations

    animation as vs a = Animation <$> (mapM (samplers ∘ channels) =≪ findJoints)
      where
        findJoints = (.joints) <$> ((!? 0) =≪ g.skins)
        channels j = V.filter ((≡ Just j) ∘ (.target.node)) a.channels

        samplers cs = AnimationSamplers
          <$> (sampler =≪ findByPath TRANSLATION cs)
          <*> (sampler =≪ findByPath ROTATION cs)
          <*> (sampler =≪ findByPath SCALE cs)

          where findByPath p = find ((≡ p) ∘ (.target.path))

        sampler ∷ Unparsed.AnimationChannel → Maybe (AnimationSampler α)
        sampler = parseSampler ↢ findSampler
          where
            findSampler c = a.samplers !? c.sampler.unAnimationSamplerIx
            parseSampler s = AnimationSampler
              <$> (bufferView vs =≪ as !? s.input.unAccessorIx)
              <*> (bufferView vs =≪ as !? s.output.unAccessorIx)

    bufferView ∷ Vector Unparsed.BufferView → Unparsed.Accessor → Maybe (BufferView α)
    bufferView vs = fmap parseBufferView ∘ ((vs !?) ∘ (.unBufferViewIx)) ↢ (.bufferView)
      where parseBufferView v = BufferView v.buffer.unBufferIx v.byteLength v.byteOffset

loadBuffers ∷ GlTF → IO Buffers
loadBuffers g = mapM loadBuffer g.buffers
  where
    loadBuffer = fromEither ∘ mapLeft stringException ↢ loadURI (fmap pure ∘ readFile)

access ∷ Binary α ⇒ Buffers → BufferView α → Either String [α]
access bs v = (maybeToEither "Data not found" ∘ fmap decode) buffer
  where
    buffer = bs !? v.buffer
    decode = decodeMany ∘ fromStrict ∘ B.take v.length ∘ B.drop v.offset
