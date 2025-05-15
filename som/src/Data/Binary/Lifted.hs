module Data.Binary.Lifted (decodeFile) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Binary (Binary)
import Data.Binary qualified as Unlifted (decodeFile)

decodeFile ∷ (Binary α, MonadIO μ) ⇒ FilePath → μ α
decodeFile = liftIO ∘ Unlifted.decodeFile
