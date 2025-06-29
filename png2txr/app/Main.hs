module Main where

import SOM.Prelude

import SOM.CLI (Options (..), fromEither, outputOrExtension, parseOptions, runCLI)
import SOM.Binary.Texture (Texture (..))

import Codec.Picture (Image (..), convertRGBA8, readImage)
import Codec.Picture.Types (PixelRGBA8 (..), pixelFoldMap)

import Data.Binary (encodeFile)
import Data.Monoid (Any (..))
import Data.Vector.Storable (toList)

main ∷ IO ()
main = runCLI do
  o ← parseOptions

  i ← (fromEither ↢ readImage) o.input

  (encodeFile (outputOrExtension "txr" o) ∘ texture ∘ convertRGBA8) i

  where
    texture i = case i of
      Image w h xs → Texture (fromIntegral w) (fromIntegral h) (transparent i) (toList xs)
    transparent = getAny ∘ pixelFoldMap (\ case (PixelRGBA8 _ _ _ a) → Any (0 ≤ a ∧ a ≤ 1))
