module SOM.Binary.Animated
  ( Animation (..)
  , Joint (..)
  , Keyframe (..)
  , Model (..)
  , Transformation (..)
  , Vertex (..)
  ) where

import SOM.Prelude

import SOM.Binary.Bounds (Bounds)

import Data.Binary (Binary)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word16)

import Foreign.Storable.Generic (GStorable)

import GHC.Generics (Generic)

import Linear.Matrix (M44)
import Linear.Quaternion (Quaternion)
import Linear.V2 (V2)
import Linear.V3 (V3)
import Linear.V4 (V4)

data Model = Model { vertices  ∷ [Vertex]
                   , indices   ∷ [Word16]
                   , joints    ∷ [Joint]
                   , bounds    ∷ Bounds
                   , texture   ∷ FilePath
                   , animation ∷ Animation
                   } deriving Generic

instance Binary Model

data Vertex = Vertex { position ∷ V3 Float
                     , normal   ∷ V3 Float
                     , texCoord ∷ V2 Float
                     , joints   ∷ V4 Word16
                     , weights  ∷ V4 Float
                     } deriving Generic

instance Binary Vertex
instance GStorable Vertex

data Joint = Joint { transformation    ∷ M44 Float
                   , inverseBindMatrix ∷ M44 Float
                   } deriving Generic

instance Binary Joint

newtype Animation = Animation { transformations ∷ [Transformation] } deriving Generic

instance Binary Animation


data Transformation = Transformation { translation ∷ NonEmpty (Keyframe (V3 Float))
                                     , rotation    ∷ NonEmpty (Keyframe (Quaternion Float))
                                     , scale       ∷ NonEmpty (Keyframe (V3 Float))
                                     } deriving Generic

instance Binary Transformation

data Keyframe α = Keyframe { time  ∷ Double
                           , value ∷ α
                           } deriving Generic

instance Binary α ⇒ Binary (Keyframe α)
