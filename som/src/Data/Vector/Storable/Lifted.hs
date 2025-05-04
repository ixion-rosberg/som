module Data.Vector.Storable.Lifted (unsafeWith) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Unlifted (unsafeWith)

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

unsafeWith ∷ (Storable α, MonadIO μ) ⇒ Vector α → (Ptr α → IO β) → μ β
unsafeWith = (liftIO ∘) ∘ Unlifted.unsafeWith
