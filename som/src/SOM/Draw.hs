module SOM.Draw (Draw (..)) where

import UnliftIO (MonadUnliftIO)

newtype Draw = Draw (∀ μ. MonadUnliftIO μ ⇒ μ ())
