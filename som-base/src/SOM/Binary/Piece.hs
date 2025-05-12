module SOM.Binary.Piece (Model (..), Vertex (..)) where

import SOM.Prelude

import Data.Binary (Binary)
import Data.Word (Word16)

import Foreign.Storable.Generic (GStorable)

import GHC.Generics (Generic)

import Linear.V2 (V2)
import Linear.V3 (V3)

data Model = Model { vertices ∷ [Vertex]
                   , indices  ∷ [Word16]
                   } deriving Generic

instance Binary Model

data Vertex = Vertex { position ∷ V3 Float
                     , normal   ∷ V3 Float
                     , texCoord ∷ V2 Float
                     } deriving Generic

instance Binary Vertex
instance GStorable Vertex
