module Main where

import SOM.Prelude

import SOM.CLI (Options (..), handlers, outputOrExtension, parser)
import SOM.Binary.Texture (Texture (..))

import Codec.Picture (Image (..), convertRGBA8, readImage)
import Codec.Picture.Types (PixelRGBA8 (..), pixelFoldMap)

import Data.Binary (encodeFile)
import Data.Either.Extra (mapLeft)
import Data.Monoid (Any (..))
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

  (encodeFile (outputOrExtension "txr" o) ∘ texture ∘ convertRGBA8) i

  where lift = fromEither ∘ mapLeft stringException
        texture i = case i of
          Image w h xs → Texture (fromIntegral w) (fromIntegral h) (transparent i) (toList xs)
        transparent = getAny ∘ pixelFoldMap (\ case (PixelRGBA8 _ _ _ a) → Any (0 ≤ a ∧ a ≤ 1))
