module Main where

import SOM.Prelude

import SOM.Binary.Piece (CollisionShape (..), Face (..), Triangle (..))
import SOM.CLI (Options (..), handlers, outputOrExtension, parser)
import SOM.GlTF (Accessor (..), GlTF (..), Mesh (..), access, loadBuffers, parse)
import SOM.Normal (Normal (..))

import Codec.GlTF (fromFile)

import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.List ((!?))

import Options.Applicative (execParser, fullDesc, info)

import UnliftIO.Exception
  ( catches
  , fromEither
  , stringException
  )

main ∷ IO ()
main = (flip catches) handlers do
  o ← execParser $ info parser fullDesc

  g ← (lift ∘ parse ↢ lift ↢ fromFile) o.input

  b ← loadBuffers g

  ps ← lift $ access b g.mesh.position.bufferView
  ns ← lift $ access b g.mesh.normal
  is ← lift $ access b g.mesh.indices

  let ps' = (fmap ∘ fmap) (.unIEEE) ps
      ns' = (Normal ∘ fmap (.unIEEE)) <$> ns
      is' = (fromIntegral ∘ (.unUnsignedShort)) <$> is

  c ← note $ CollisionShape <$> faces ps' ns' is'

  encodeFile (outputOrExtension "mhm" o) c

  where
    lift = fromEither ∘ mapLeft stringException
    note = fromEither ∘ maybeToEither (stringException "Invalid collision shape")

    faces _  _  []         = Just []
    faces ps ns (a:b:c:fs) = (:) <$> face ps ns a b c <*> faces ps ns fs
    faces _  _  _          = Nothing

    face ps ns a b c = Face <$> ns !? a <*> (Triangle <$> ps !? a <*> ps !? b <*> ps !? c)
