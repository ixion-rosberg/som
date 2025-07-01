module FRP.Yampa.Extra (appendEvents, clampedIntegral, concatEvents, foldMapEvents, integrateBy) where

import SOM.Prelude

import Data.Ord (clamp)

import FRP.Yampa (Event (..), SF, iterFrom, mergeBy)

integrateBy ∷ Fractional α ⇒ (α → β → β) → β → SF α β
integrateBy f = iterFrom (\ x _ dt x₀ → f (realToFrac dt × x) x₀)

clampedIntegral ∷ (Ord α, Fractional α) ⇒ (α, α) → SF α α
clampedIntegral c = integrateBy ((clamp c ∘ ) ∘ (+)) 0.0

appendEvents ∷ Monoid α ⇒ Event α → Event α → Event α
appendEvents = mergeBy (<>)

concatEvents ∷ (Monoid α, Foldable τ) ⇒ τ (Event α) → Event α
concatEvents = foldr appendEvents NoEvent

foldMapEvents ∷ (Monoid β, Functor τ, Foldable τ) ⇒ (α → β) → τ (Event α) → Event β
foldMapEvents f xs = concatEvents ((fmap ∘ fmap) f xs)
