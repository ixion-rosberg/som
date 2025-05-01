module SOM.Binary.Piece (Model (..), Vertex (..)) where

import Data.Binary (Binary)
import Data.Word (Word16)

import GHC.Generics (Generic)

import Linear.V2 (V2)
import Linear.V3 (V3)

data Model = Model [Vertex] [Index] deriving Generic

instance Binary Model

data Vertex = Vertex Position Normal TexCoord deriving Generic

instance Binary Vertex

type Position = V3 Float
type Normal = V3 Float
type TexCoord = V2 Float

type Index = Word16
