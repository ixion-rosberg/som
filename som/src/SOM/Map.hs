module SOM.Map (Map, Orientation (..), Piece (..), PieceSetup (..), create, pieces) where

import SOM.Prelude

import SOM.Renderer.Model (Model)
import SOM.Renderer.Texture (Texture)

import Data.Map.Strict (elems, fromList)
import Data.Map.Strict qualified as M (Map)

import Linear.Matrix (M44, mkTransformation)
import Linear.Quaternion (axisAngle)
import Linear.V3 (V3 (..))

data Map = Map (M.Map (ℕ, ℕ) Piece)

data Piece = Piece { model ∷ Model, texture ∷ Texture, transformation ∷ M44 Float }

data PieceSetup = PieceSetup { model ∷ Model, texture ∷ Texture, x ∷ ℕ, z ∷ ℕ, orientation ∷ Orientation }

data Orientation = South | East | North | West

create ∷ [PieceSetup] → Map
create = Map ∘ fromList ∘ fmap piece
  where
    piece p = ((p.x, p.z), Piece p.model p.texture transformation)
      where
        transformation = mkTransformation orientation position
        orientation = axisAngle (V3 0 1 0) case p.orientation of
          South → 0
          East  → π ÷ 2
          North → π
          West  → π × 3 ÷ 2
        position = V3 (size × fromIntegral p.x) 0 -(size × fromIntegral p.z)
        size = 2

pieces ∷ Map → [Piece]
pieces (Map m) = elems m
