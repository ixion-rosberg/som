module SOM.Prelude
  ( module P
  , module U
  , (⤛)
  , (⤜)
  ) where

import Prelude as P hiding
  ( head
  , init
  , last
  , tail
  , (!!)
  )

import Prelude.Unicode as U hiding ((‼))

import Control.Monad.Unicode as U

(⤛) ∷ Monad μ ⇒ (α → μ β) → μ α → μ β
(⤛) = (=<<)

infixr 1 ⤛

(⤜) ∷ Monad μ ⇒ μ α → (α → μ β) → μ β
(⤜) = (>>=)

infixl 1 ⤜
