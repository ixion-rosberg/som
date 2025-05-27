module SOM.Map
  ( Map
  , Orientation (..)
  , Piece (..)
  , PieceSetup (..)
  , collisionShapes
  , create
  , pieces
  ) where

import SOM.Prelude

import SOM.Binary.Piece (CollisionShape)
import SOM.Renderer.Draw (Draw (..))
import SOM.Transform (transform)

import Data.Map.Strict (elems, fromList)
import Data.Map.Strict qualified as M (Map)

import Linear.Matrix (M44, mkTransformation)
import Linear.Quaternion (axisAngle)
import Linear.V3 (V3 (..))

data Map = Map (M.Map (ℕ, ℕ) Piece)

data Piece = Piece { draw           ∷ Draw
                   , collisionShape ∷ CollisionShape
                   , transformation ∷ M44 Float
                   }

data PieceSetup = PieceSetup { draw           ∷ M44 Float → Draw
                             , collisionShape ∷ CollisionShape
                             , x              ∷ ℕ
                             , z              ∷ ℕ
                             , orientation    ∷ Orientation
                             }

data Orientation = South | East | North | West

create ∷ [PieceSetup] → Map
create = Map ∘ fromList ∘ fmap piece
  where
    piece p = ((p.x, p.z), Piece (p.draw transformation) p.collisionShape transformation)
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

collisionShapes ∷ Map → [CollisionShape]
collisionShapes = fmap (\ p → transform p.transformation p.collisionShape) ∘ pieces
