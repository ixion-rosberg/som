module Main where

import SOM.Prelude

import SOM.CLI (Options (..), handlers, outputOrExtension, parser)
import SOM.Binary.Texture (Texture (..))

import Codec.Picture (Image (..), convertRGBA8, readImage)

import Data.Binary (encodeFile)
import Data.Either.Extra (mapLeft)
import Data.Vector.Storable (toList)

import Options.Applicative (execParser, fullDesc, info)

import UnliftIO.Exception
  ( catches
  , fromEither
  , stringException
  )

main ∷ IO ()
main = (flip catches) handlers do

  o ← execParser $ info parser fullDesc

  i ← (lift ↢ readImage) o.input

  encodeFile (outputOrExtension "txr" o) (texture i)

  where lift = fromEither ∘ mapLeft stringException
        texture i = case convertRGBA8 i of
          Image w h xs → Texture (fromIntegral w) (fromIntegral h) (toList xs)
