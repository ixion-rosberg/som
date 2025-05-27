module SOM.IdentityList (IdentityList, fromList, toList) where

import SOM.Prelude

import Data.Map.Strict (Map, elems)
import Data.Map.Strict qualified as M (insert)

data IdentityList α = IdentityList ℕ (Map ℕ α) deriving (Functor, Foldable)

fromList ∷ [α] → IdentityList α
fromList = foldr insert (IdentityList 0 mempty)

insert ∷ α → IdentityList α → IdentityList α
insert x (IdentityList n xs) = IdentityList (n + 1) (M.insert n x xs)

toList ∷ IdentityList α → [α]
toList (IdentityList _ xs) = elems xs
