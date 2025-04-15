module System.IO.Lifted (readFile) where

import SOM.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified System.IO as Unlifted (readFile)

readFile ∷ MonadIO μ ⇒ FilePath → μ String
readFile = liftIO ∘ Unlifted.readFile
