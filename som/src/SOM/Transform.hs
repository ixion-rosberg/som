module SOM.Transform (Transform (..)) where

import SOM.Prelude

import SOM.Binary.Piece (CollisionShape (..), Face (..), Triangle (..))
import SOM.Normal (Normal (..))

import Control.Lens (view)

import Data.Coerce (coerce)

import Linear.Matrix (M44, _m33, inv44, transpose, (!*))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

class Transform α where
  transform ∷ M44 Float → α → α

instance Transform (V3 Float) where
  transform m (V3 x y z) = V3 x' y' z'
    where (V4 x' y' z' _) = m !* V4 x y z 1

instance Transform Face where
  transform m f = Face (transform m f.normal) (transform m f.triangle)

instance Transform (Normal Float) where
  transform m n = m' !* n
    where m' = (coerce ∘ view _m33 ∘ transpose ∘ inv44) m

instance Transform Triangle where
  transform m t = Triangle (transform m t.a) (transform m t.b) (transform m t.c)

instance Transform CollisionShape where
  transform m c = CollisionShape (transform m <$> c.faces)
