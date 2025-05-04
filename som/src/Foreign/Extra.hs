module Foreign.Extra (queries, query, sizeOf) where

import SOM.Prelude

import UnliftIO (MonadUnliftIO)

import Foreign (Storable, Ptr)
import Foreign qualified as Foreign (sizeOf)
import Foreign.Lifted (peek)

import UnliftIO.Foreign (alloca)

query ∷ (Storable α, MonadUnliftIO μ) ⇒ (Ptr α → μ ()) → μ α
query f = alloca \ ptr → f ptr *> peek ptr

queries ∷ (Storable α, MonadUnliftIO μ) ⇒ (α → β) → (Ptr α → μ ()) → μ β
queries f = fmap f ∘ query

sizeOf ∷ ∀ α → Storable α ⇒ Int
sizeOf a = Foreign.sizeOf (undefined ∷ a)
