{-# OPTIONS_GHC -Wno-type-defaults #-}

module SOM.Collision (BoundingBox (..), BoundingSphere (..), Collision (..), Ray (..), (╳)) where

import SOM.Prelude

import SOM.Binary.Piece (CollisionShape (..), Face (..), Triangle (..))
import SOM.Direction (Direction (..))

import Control.Monad (guard)

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Ord (clamp)

import Linear.Metric (distance, normalize)
import Linear.Metric.Unicode ((⋅))
import Linear.V3 (V3 (..))
import Linear.V3.Unicode qualified as V3 ((×))
import Linear.Vector ((*^))

data BoundingSphere = BoundingSphere { center ∷ V3 Float
                                     , radius ∷ Float
                                     }

data BoundingBox = BoundingBox { min ∷ V3 Float
                               , max ∷ V3 Float
                               }

data Collision = Collision { point  ∷ V3 Float
                           , normal ∷ Direction Float
                           }

data Plane = Plane { normal   ∷ V3 Float
                   , distance ∷ Float
                   }

data Line = Line { a ∷ V3 Float
                 , b ∷ V3 Float
                 }

data Ray = Ray { origin    ∷ V3 Float
               , direction ∷ Direction Float
               }

class α ╳ β where
  type Result α β
  (╳) ∷ α → β → Result α β

instance BoundingSphere ╳ CollisionShape where
  type Result BoundingSphere CollisionShape = [Collision]

  (╳) s = catMaybes ∘ fmap (s ╳) ∘ (.faces)

instance BoundingSphere ╳ Face where
  type Result BoundingSphere Face = Maybe Collision

  s ╳ f = guard intersecting $> Collision p f.normal
    where
      intersecting = distance p s.center ^ 2 < s.radius ^ 2
      p = closestPoint s.center f.triangle

instance Ray ╳ BoundingBox where
  type Result Ray BoundingBox = Bool

  r ╳ b = maximum tmin ≤ minimum tmax
    where t₁   = (b.min - r.origin) ÷ r.direction.unDirection
          t₂   = (b.max - r.origin) ÷ r.direction.unDirection
          tmin = min <$> t₁ <*> t₂
          tmax = max <$> t₁ <*> t₂

plane ∷ Triangle → Plane
plane t = Plane normal (normal ⋅ t.a)
  where normal = normalize ((t.b - t.a) V3.× (t.c - t.a))

class ClosestPoint α where
  closestPoint ∷ V3 Float → α → V3 Float

instance ClosestPoint Triangle where
  closestPoint x t = minimumBy (compare `on` ((^ 2) ∘ distance p)) (p₁ :| [ p₂, p₃ ])
        where p  = closestPoint x (plane t)
              p₁ = closestPoint x (Line t.a t.b)
              p₂ = closestPoint x (Line t.b t.c)
              p₃ = closestPoint x (Line t.c t.a)

instance ClosestPoint Plane where
  closestPoint x p = x - ((p.normal ⋅ x - p.distance) *^ p.normal)

instance ClosestPoint Line where
  closestPoint x l = l.a + t' *^ v
        where v  = l.b - l.a
              t  = ((x - l.a) ⋅ v) ÷ (v ⋅ v)
              t' = clamp (0, 1) t
