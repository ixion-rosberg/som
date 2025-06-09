module SOM.Binary.Piece
  ( CollisionShape (..)
  , Face (..)
  , Model (..)
  , Triangle (..)
  , Vertex (..)
  ) where

import SOM.Prelude

import SOM.Direction (Direction)

import Data.Binary (Binary)
import Data.Word (Word16)

import Foreign.Storable.Generic (GStorable)

import GHC.Generics (Generic)

import Linear.V2 (V2)
import Linear.V3 (V3)

data Model = Model { vertices ∷ [Vertex]
                   , indices  ∷ [Word16]
                   , texture  ∷ FilePath
                   } deriving Generic

instance Binary Model

data Vertex = Vertex { position ∷ V3 Float
                     , normal   ∷ V3 Float
                     , texCoord ∷ V2 Float
                     } deriving Generic

instance Binary Vertex
instance GStorable Vertex

newtype CollisionShape = CollisionShape { faces ∷ [Face] } deriving Generic

instance Binary CollisionShape

data Face = Face { normal ∷ Direction Float, triangle ∷ Triangle } deriving Generic

instance Binary Face

data Triangle = Triangle { a ∷ V3 Float, b ∷ V3 Float, c ∷ V3 Float } deriving Generic

instance Binary Triangle
