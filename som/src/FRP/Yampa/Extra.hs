module FRP.Yampa.Extra (clampedIntegral, integrateBy) where

import SOM.Prelude

import Data.Ord (clamp)

import FRP.Yampa (SF, iterFrom)

integrateBy ∷ Fractional α ⇒ (α → β → β) → β → SF α β
integrateBy f = iterFrom (\ x _ dt x₀ → f (realToFrac dt × x) x₀)

clampedIntegral ∷ (Ord α, Fractional α) ⇒ (α, α) → SF α α
clampedIntegral c = integrateBy ((clamp c ∘ ) ∘ (+)) 0.0
