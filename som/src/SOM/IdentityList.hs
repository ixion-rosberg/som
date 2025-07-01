module SOM.IdentityList (IdentityList, adjust, assocs, delete, fromList, insert, toList) where

import SOM.Prelude

import Data.Map.Strict (Map, elems)
import Data.Map.Strict qualified as M (adjust, assocs, delete, insert)

data IdentityList α = IdentityList ℕ (Map ℕ α) deriving (Functor, Foldable)

fromList ∷ [α] → IdentityList α
fromList = foldr insert (IdentityList 0 mempty)

insert ∷ α → IdentityList α → IdentityList α
insert x (IdentityList n xs) = IdentityList (n + 1) (M.insert n x xs)

delete ∷ ℕ → IdentityList α → IdentityList α
delete i (IdentityList n xs) = IdentityList n (M.delete i xs)

adjust ∷ (α → α) → ℕ → IdentityList α → IdentityList α
adjust f i (IdentityList n xs) = IdentityList n (M.adjust f i xs)

toList ∷ IdentityList α → [α]
toList (IdentityList _ xs) = elems xs

assocs ∷ IdentityList α → [(ℕ, α)]
assocs (IdentityList _ xs) = M.assocs xs
