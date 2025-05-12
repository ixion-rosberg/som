module Main where

import SOM.Prelude

import SOM.Binary.Piece (Model (..), Vertex (..))
import SOM.GlTF (GlTF (..), Mesh (..), access, loadBufferData, parse)
import SOM.Options (Options (..), outputOrExtension, parser)

import Codec.GlTF (fromFile)


import Data.Binary (encodeFile)
import Data.Binary.Extra (IEEE (..), UnsignedShort (..))
import Data.Either.Extra (mapLeft)

import Options.Applicative (execParser, fullDesc, info)

import UnliftIO.Exception
  ( Handler (..)
  , IOException
  , StringException (..)
  , catches
  , fromEither
  , stringException
  )

data Options = Options { input ∷ String, output ∷ Maybe String }


main ∷ IO ()
main = (flip catches) handlers do
      o ← execParser $ info parser fullDesc

      g ← (lift ∘ parse ↢ lift ↢ fromFile) o.input

      b ← loadBufferData g

      ps ← lift $ access b g.mesh.position
      ns ← lift $ access b g.mesh.normal
      ts ← lift $ access b g.mesh.texCoord
      is ← lift $ access b g.mesh.indices

      let vs = zipWith3 vertex ps ns ts
          is' = (.unUnsignedShort) <$> is

      encodeFile (outputOrExtension "msm" o) (Model vs is')

      pure ()

  where
    handlers = [ Handler (\ (StringException e _) → putStrLn e)
               , Handler (\ (e ∷ IOException) → print e)
               ]
    lift = fromEither ∘ mapLeft stringException
    vertex p n t = Vertex ((.unIEEE) <$> p) ((.unIEEE) <$> n) ((.unIEEE) <$> t)
