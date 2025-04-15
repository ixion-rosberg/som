module Foreign.Lifted (peek) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign (Ptr, Storable)
import Foreign qualified as Unlifted (peek)

peek ∷ (Storable α, MonadIO μ) ⇒ Ptr α → μ α
peek = liftIO ∘ Unlifted.peek
