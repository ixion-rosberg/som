module Linear.V3.Unicode ((×)) where

import SOM.Prelude hiding ((×))

import Linear.V3 (V3, cross)

(×) ∷ Num α ⇒ V3 α → V3 α → V3 α
(×) = cross

infix 7 ×
