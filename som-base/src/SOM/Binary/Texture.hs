module SOM.Binary.Texture (Texture (..)) where

import SOM.Prelude

import Data.Binary (Binary)
import Data.Int (Int32)
import Data.Word (Word8)

import GHC.Generics (Generic)

data Texture = Texture { width       ∷ Int32
                       , height      ∷ Int32
                       , transparent ∷ Bool
                       , image       ∷ [Word8]
                       } deriving Generic

instance Binary Texture
