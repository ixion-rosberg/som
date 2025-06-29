module Main where

import SOM.Prelude

import SOM.Binary.Piece (CollisionShape (..), Face (..), Triangle (..))
import SOM.CLI (Options (..), fromEither, outputOrExtension, parseOptions, runCLI)
import SOM.Direction (Direction (..))
import SOM.GlTF (Accessor (..), GlTF (..), Mesh (..), access, fromFile, loadBuffers)

import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Coerce (coerce)
import Data.Either.Extra (maybeToEither)
import Data.List ((!?))

main ∷ IO ()
main = runCLI do
  o ← parseOptions

  g ← (fromEither ↢ fromFile) o.input
  b ← loadBuffers g

  c ← fromEither (collisionShape b g.mesh)

  encodeFile (outputOrExtension "mhm" o) c

  where
    collisionShape b m = do
      ps ← access b m.position.bufferView
      ns ← access b m.normal
      is ← access b m.indices

      fs ← note (faces (coerce ps) (coerce ns) (indices is))

      pure (CollisionShape fs)

    note = maybeToEither "Invalid collision shape"

    indices = fmap (fromIntegral ∘ (.unUnsignedShort))

    faces _  _  []         = Just []
    faces ps ns (a:b:c:fs) = (:) <$> face ps ns a b c <*> faces ps ns fs
    faces _  _  _          = Nothing

    face ps ns a b c = Face <$> ns !? a <*> (Triangle <$> ps !? a <*> ps !? b <*> ps !? c)
