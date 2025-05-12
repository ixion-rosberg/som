module SOM.Binary.Texture (Texture (..)) where

import Data.Int (Int32)
import Data.Vector.Storable (Vector)
import Data.Word (Word8)

data Texture = Texture { width  ∷ Int32
                       , height ∷ Int32
                       , image  ∷ Vector Word8
                       }
