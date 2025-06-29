module Main where

import SOM.Prelude

import SOM.Binary.Piece (Model (..), Vertex (..))
import SOM.GlTF (Accessor (..), GlTF (..), Image (..), Mesh (..), access, fromFile, loadBuffers)
import SOM.CLI (Options (..), fromEither, outputOrExtension, parseOptions, runCLI)

import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Coerce (coerce)

main ∷ IO ()
main = runCLI do
  o ← parseOptions

  g ← (fromEither ↢ fromFile) o.input
  b ← loadBuffers g
  m ← fromEither (model b g)

  encodeFile (outputOrExtension "msm" o) m
  where
    model b g = Model
      <$> vertices b g.mesh
      <*> indices  b g.mesh
      <#> texture    g.image

    vertices b m = zipWith3 vertex
      <$> access b m.position.bufferView
      <*> access b m.normal
      <*> access b m.texCoord

    vertex p n t = Vertex (coerce p) (coerce n) (coerce t)

    indices b m = coerce <$> access b m.indices

    texture i = i.name <> ".txr"
