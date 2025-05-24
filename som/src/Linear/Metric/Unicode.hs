module Linear.Metric.Unicode ((⋅)) where

import SOM.Prelude

import Linear.Metric (Metric, dot)


(⋅) ∷ (Num α, Metric φ) ⇒ φ α → φ α → α
(⋅) = dot

infix 7 ⋅
