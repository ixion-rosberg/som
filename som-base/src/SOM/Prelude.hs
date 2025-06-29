module SOM.Prelude
  ( module E
  , (^≫)
  , (^≪)
  , (≫^)
  , (≪^)
  , (<#>)
  ) where

import Prelude as E hiding
  ( error
  , head
  , init
  , last
  , lookup
  , return
  , readFile
  , tail
  , (!!)
  , (>>)
  )

import Prelude.Unicode as E hiding ((‼), (⋅))

import Control.Arrow (Arrow, (^>>), (^<<), (>>^), (<<^))
import Control.Arrow.Unicode as E
import Control.Monad.Unicode as E hiding ((≫))

import Data.Functor as E (($>))

import Numeric.Natural.Unicode as E (ℕ)

(^≫) ∷ Arrow α ⇒ (β → γ) → α γ δ → α β δ
(^≫) = (^>>)

infixr 1 ^≫

(^≪) ∷ Arrow α ⇒ (γ → δ) → α β γ → α β δ
(^≪) = (^<<)

infixr 1 ^≪

(≫^) ∷ Arrow α ⇒ α β γ → (γ → δ) → α β δ
(≫^) = (>>^)

infixr 1 ≫^

(≪^) ∷ Arrow α ⇒ α γ δ → (β → γ) → α β δ
(≪^) = (<<^)

infixr 1 ≪^

(<#>) ∷ Functor φ ⇒ φ (α → β) → α → φ β
f <#> x = ($ x) <$> f

infixl 4 <#>
