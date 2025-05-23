module SOM.GlTF (BufferData, GlTF (..), Mesh (..), access, loadBufferData, parse) where

import SOM.Prelude

import Codec.GlTF qualified as Unparsed (GlTF (..))
import Codec.GlTF.Accessor qualified as Unparsed (Accessor (..), AccessorIx (..))
import Codec.GlTF.Buffer qualified as Unparsed (Buffer (..), BufferIx (..))
import Codec.GlTF.BufferView (BufferView (..))
import Codec.GlTF.BufferView qualified as Unparsed (BufferViewIx (..))
import Codec.GlTF.Mesh qualified as Unparsed (Mesh (..), MeshPrimitive (..))
import Codec.GlTF.URI (URI, loadURI)

import Data.Binary (Binary)
import Data.Binary.Extra (IEEE, UnsignedShort, decodeMany)
import Data.ByteString (ByteString, fromStrict, readFile)
import Data.ByteString qualified as B (drop, take)
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.HashMap.Strict (lookup)
import Data.Text (Text, unpack)
import Data.Vector (Vector, (!?))

import Linear.V2 (V2)
import Linear.V3 (V3)

import UnliftIO.Exception (fromEither, stringException)

data GlTF = GlTF { buffers     ∷ Vector URI
                 , bufferViews ∷ Vector BufferView
                 , accessors   ∷ Vector Accessor
                 , mesh        ∷ Mesh
                 }

data Mesh = Mesh { position ∷ AccessorIx (V3 IEEE)
                 , normal   ∷ AccessorIx (V3 IEEE)
                 , texCoord ∷ AccessorIx (V2 IEEE)
                 , indices  ∷ AccessorIx UnsignedShort
                 }

data BufferData = BufferData { buffers     ∷ Vector ByteString
                             , bufferViews ∷ Vector BufferView
                             , accessors   ∷ Vector Accessor
                             }

newtype AccessorIx α = AccessorIx { unAccessorIx ∷ Int }

type Accessor = Int

parse ∷ Unparsed.GlTF → Either String GlTF
parse g = GlTF
  <$> (mapM parseBuffer =≪ buffers g)
  <*> bufferViews g
  <*> (mapM parseAccessor =≪ accessors g)
  <*> (parseMesh =≪ mesh g)

  where
    buffers = maybeToEither "Missing buffers" ∘ (.buffers)
    parseBuffer = maybeToEither "Missing URI" ∘ (.uri)
    bufferViews = maybeToEither "Missing buffer views" ∘ (.bufferViews)
    accessors = maybeToEither "Missing accessors" ∘ (.accessors)
    parseAccessor = maybeToEither "Missing buffer view" ∘ fmap (.unBufferViewIx) ∘ (.bufferView)
    mesh = maybeToEither "Missing mesh" ∘ ((!? 0) ∘ (.primitives) ↢ (!? 0) ↢ (.meshes))
    parseMesh m = Mesh
      <$> attribute "POSITION"
      <*> attribute "NORMAL"
      <*> attribute "TEXCOORD_0"
      <*> indices
      where
        attribute ∷ Text → Either String (AccessorIx α)
        attribute a = getIndex ("Missing " <> unpack a) (lookup a m.attributes)
        indices = getIndex "Missing indices" m.indices
        getIndex e = maybeToEither e ∘ fmap (AccessorIx ∘ (.unAccessorIx))

loadBufferData ∷ GlTF → IO BufferData
loadBufferData g = do
  bs ← mapM loadBuffer g.buffers
  pure (BufferData bs g.bufferViews g.accessors)
  where
    loadBuffer = fromEither ∘ mapLeft stringException ↢ loadURI (fmap pure ∘ readFile)

access ∷ Binary α ⇒ BufferData → AccessorIx α → Either String [α]
access b i = maybeToEither "Data not found" do
  v ← bufferView
  decode v <$> buffer v
  where
    bufferView = (b.bufferViews !?) =≪ b.accessors !? i.unAccessorIx
    buffer v = b.buffers !? v.buffer.unBufferIx
    decode v = decodeMany ∘ fromStrict ∘ B.take v.byteLength ∘ B.drop v.byteOffset
