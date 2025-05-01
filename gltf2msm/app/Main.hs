module Main where

import SOM.Prelude

import Codec.GlTF (GlTF (..), fromFile)
import Codec.GlTF.Accessor (Accessor (..), AccessorIx (..))
import Codec.GlTF.Buffer (Buffer (..), BufferIx (..))
import Codec.GlTF.BufferView (BufferView (..), BufferViewIx (..))
import Codec.GlTF.Mesh (Mesh (..), MeshPrimitive (..))
import Codec.GlTF.URI (loadURI)

import Data.Binary (Binary)
import Data.Binary.Extra (IEEE, UnsignedShort, decodeMany)
import Data.ByteString (ByteString, fromStrict, readFile)
import Data.ByteString qualified as B (drop, take)
import Data.Either.Extra (mapLeft)
import Data.HashMap.Strict (lookup)
import Data.Text (Text, unpack)
import Data.Vector (Vector, (!?))

import Linear.V2 (V2)
import Linear.V3 (V3)

import Options.Applicative (Parser, argument, execParser, fullDesc, info, metavar, str)

import UnliftIO.Exception (Handler (..), IOException, StringException (..), catches, fromEither, stringException)
import UnliftIO.Exception.Extra (fromMaybe)

parser ∷ Parser FilePath
parser = argument str (metavar "FILE")

data BufferData = BufferData { buffers ∷ Vector ByteString
                             , bufferViews ∷ Vector BufferView
                             , accessors ∷ Vector Accessor
                             }

main ∷ IO ()
main = (flip catches) handlers do
      f ← execParser $ info parser fullDesc

      g ← fromEither ∘ mapLeft stringException =≪ fromFile f

      b ← loadBufferData g

      m ← fromMaybe (stringException "Missing mesh") (mesh g)

      (ps ∷ [V3 IEEE]) ← accessMeshAttribute b "POSITION" m
      (ns ∷ [V3 IEEE]) ← accessMeshAttribute b "NORMAL" m
      (ts ∷ [V2 IEEE]) ← accessMeshAttribute b "TEXCOORD_0" m
      (is ∷ [V3 UnsignedShort]) ← accessIndices b m

      pure ()

  where
    handlers = [ Handler (\ (StringException e _) → putStrLn e)
               , Handler (\ (e ∷ IOException) → print e)
               ]

    mesh = (!? 0) ∘ (.primitives) ↢ (!? 0) ↢ (.meshes)

loadBufferData ∷ GlTF → IO BufferData
loadBufferData g = BufferData <$> buffers <*> bufferViews <*> accessors
  where
    buffers = loadBuffers =≪ fromMaybe (stringException "Missing buffers") g.buffers
    bufferViews = fromMaybe (stringException "Missing buffer views") g.bufferViews
    accessors = fromMaybe (stringException "Missing accessors") g.accessors

    loadBuffers = mapM (loadBuffer ↢ uri)
    uri = fromMaybe (stringException "Missing URI") ∘ (.uri)
    loadBuffer = fromEither ∘ mapLeft stringException ↢ loadURI (fmap pure ∘ readFile)

accessMeshAttribute ∷ Binary α ⇒ BufferData → Text → MeshPrimitive → IO [α]
accessMeshAttribute b a m = access b =≪ accessor
  where accessor = fromMaybe ex (lookup a m.attributes)
        ex = stringException ("Missing attribute " <> unpack a)

accessIndices ∷ BufferData → MeshPrimitive → IO [V3 UnsignedShort]
accessIndices b m = access b =≪ accessor
  where accessor = fromMaybe (stringException "Missing indices") m.indices

access ∷ Binary α ⇒ BufferData → AccessorIx → IO [α]
access b i = fromMaybe (stringException "Data not found") (readData =≪ bufferView)
  where
    bufferView = (b.bufferViews !?) ∘ (.unBufferViewIx) =≪ (.bufferView) =≪ b.accessors !? i.unAccessorIx
    readData v = decodeMany ∘ fromStrict ∘ bytes v <$> buffer v
    bytes v = B.take v.byteLength ∘ B.drop v.byteOffset
    buffer v = b.buffers !? v.buffer.unBufferIx
